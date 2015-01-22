{-# LANGUAGE ConstraintKinds #-}
module Databrary.Types.Identity
  ( Identity(..)
  , IdentityM
  ) where

import Control.Monad.Has (Has(..), HasM)
import Databrary.Model.Types.Authorize

data Identity = Identity
  { identityAuthorization :: Authorization
  , identitySuperuser :: Bool
  }

type IdentityM c m = HasM Identity c m

instance Has Access Identity where
  had = had . identityAuthorization
