{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Mail
  ( sendMail
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Network.Mail.Mime

import Databrary.Model.Party

baseMail :: Mail
baseMail = emptyMail (Address (Just "Databrary") "help@databrary.org")

sendMail :: MonadIO m => [Either T.Text Account] -> T.Text -> BSL.ByteString -> m ()
sendMail to subj body =
  liftIO $ renderSendMail $ addPart [Part "text/plain; charset=utf-8" None Nothing [] body] $ baseMail
    { mailTo = map addr to
    , mailHeaders = [("Subject", subj)]
    }
  where
  addr (Left e) = Address Nothing e
  addr (Right Account{ accountEmail = email, accountParty = p }) =
    Address (Just (partyName p)) email
