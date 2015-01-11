module Databrary.Identity
  (
  ) where

import Databrary.Model.Types.Authorize

data Identity = Identity
  { identityAuthorization :: Authorization
  , identitySuperuser :: Bool
  }

getIdentity :: AppHandler Identity
getIdentity = do
  getSession 
