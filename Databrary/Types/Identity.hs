{-# LANGUAGE ConstraintKinds #-}
module Databrary.Types.Identity
  ( Identity(..)
  , IdentityM
  ) where

import Control.Monad.Has (HasM)
import Databrary.Model.Types.Authorize

data Identity = Identity
  { identityAuthorization :: Authorization
  , identitySuperuser :: Bool
  }

type IdentityM = HasM Identity
