module Databrary.Types.Identity
  ( Identity(..)
  ) where

import Databrary.Model.Types.Authorize

data Identity = Identity
  { identityAuthorization :: Authorization
  , identitySuperuser :: Bool
  }
