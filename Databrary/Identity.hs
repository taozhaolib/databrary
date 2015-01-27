{-# LANGUAGE OverloadedStrings #-}
module Databrary.Identity
  ( module Databrary.Types.Identity
  , getIdentity
  ) where

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

makeIdentity :: SessionToken -> Identity
makeIdentity tok = Identity
  { identityAuthorization = sessionAuthorization tok
  , identitySuperuser = sessionSuperuser tok
  }

getIdentity :: AppM r Identity
getIdentity = do
  c <- getSignedCookie "session"
  s <- maybe (return Nothing) lookupSession c
  return $ maybe nobodyIdentity makeIdentity s
