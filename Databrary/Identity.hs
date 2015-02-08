{-# LANGUAGE OverloadedStrings #-}
module Databrary.Identity
  ( module Databrary.Identity.Types
  , getIdentity
  ) where

import Databrary.Model.Authorize
import Databrary.Model.Token
import Databrary.Action.Request
import Databrary.Resource
import Databrary.DB
import Databrary.Web.Cookie
import Databrary.Identity.Types

nobodyIdentity :: Identity
nobodyIdentity = Identity
  { identityAuthorization = PartyAuth nobodyAuthorization
  , identitySuperuser = False
  }

makeIdentity :: SessionToken -> Identity
makeIdentity tok = Identity
  { identityAuthorization = sessionAuthorization tok
  , identitySuperuser = sessionSuperuser tok
  }

getIdentity :: (MonadHasResource c m, MonadHasRequest c m, DBM m) => m Identity
getIdentity = do
  c <- getSignedCookie "session"
  s <- maybe (return Nothing) lookupSession c
  return $ maybe nobodyIdentity makeIdentity s
