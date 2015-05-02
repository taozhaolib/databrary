{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Mail
  ( sendMail
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.Mail.Mime

import Databrary.Model.Party

baseMail :: Mail
baseMail = emptyMail (Address (Just "Databrary") "help@databrary.org")

sendMail :: MonadIO m => [Either T.Text Account] -> T.Text -> TL.Text -> m ()
sendMail to subj body =
  liftIO $ renderSendMail $ addPart [plainPart body] $ baseMail
    { mailTo = map addr to
    , mailHeaders = [("Subject", subj)]
    }
  where
  addr (Left e) = Address Nothing e
  addr (Right Account{ accountEmail = email, accountParty = p }) =
    Address (Just (partyName p)) email
