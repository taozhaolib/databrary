{-# LANGUAGE OverloadedStrings #-}
module Databrary.Identity
  ( module Databrary.Types.Identity
  , getIdentity
  ) where

import qualified Data.Foldable as Fold

import Control.Has (peeks)
import Databrary.Types.Time
import Databrary.Model.Authorize
import Databrary.Model.Token
import Databrary.Action.App
import Databrary.Web.Cookie
import Databrary.Types.Identity

nobodyIdentity :: Identity
nobodyIdentity = Identity
  { identityAuthorization = PartyAuth nobodyAuthorization
  , identitySuperuser = False
  }

makeIdentity :: SessionToken -> Timestamp -> Identity
makeIdentity tok t = Identity
  { identityAuthorization = sessionAuthorization tok
  , identitySuperuser = Fold.any (t <) $ sessionSuperuser tok
  }

getIdentity :: AppM r Identity
getIdentity = do
  c <- getSignedCookie "session"
  s <- maybe (return Nothing) lookupSession c
  peeks $ maybe (return nobodyIdentity) makeIdentity s
