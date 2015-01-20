{-# LANGUAGE OverloadedStrings #-}
module Databrary.Identity
  ( module Databrary.Types.Identity
  , getIdentity
  , identityParty
  ) where

import qualified Data.Foldable as Fold

import Control.Monad.Has (HasM, pull)
import Databrary.Types.Time
import Databrary.Model.Party
import Databrary.Model.Authorize
import Databrary.Model.Token
import Databrary.Action.App
import Databrary.Web.Cookie
import Databrary.Types.Identity

nobodyIdentity :: Identity
nobodyIdentity = Identity
  { identityAuthorization = nobodyAuthorization
  , identitySuperuser = False
  }

makeIdentity :: HasM Timestamp m => SessionToken -> m Identity
makeIdentity tok = do
  t <- pull
  return $ Identity
    { identityAuthorization = sessionAuthorization tok
    , identitySuperuser = Fold.any (t <) $ sessionSuperuser tok
    }

getIdentity :: AppM r Identity
getIdentity = do
  c <- getSignedCookie "session"
  s <- maybe (return Nothing) lookupSession c
  maybe (return nobodyIdentity) makeIdentity s

identityParty :: Identity -> Party
identityParty = authorizeChild . identityAuthorization
