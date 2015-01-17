module Databrary.Identity
  ( module Databrary.Types.Identity
  , getIdentity
  ) where

import Databrary.Action
import Databrary.Web.Cookies

nobodyIdentity :: Identity
nobodyIdentity = Identity
  { identityAuthorization = nobodyAuthorization
  , identitySuperuser = False
  }

getIdentity :: ActionM Identity
getIdentity =
  fromMaybe nobodyIdentity <$> mapM getSession =<< getSignedCookie "session"
