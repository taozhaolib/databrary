{-# LANGUAGE ConstraintKinds #-}
module Databrary.Types.Identity
  ( Identity(..)
  , IdentityM
  , Auth(..)
  , makeAuth
  ) where

import Control.Monad (liftM)
import Control.Monad.Has (HasM, pull, Has(..))
import Databrary.Model.Types.Authorize

data Identity = Identity
  { identityAuthorization :: Authorization
  , identitySuperuser :: Bool
  }

type IdentityM = HasM Identity

data Auth a = Auth
  { authObject :: !a
  , authIdentity :: !Identity
  }

instance Has Identity (Auth a) where
  had = authIdentity

instance Has a b => Has a (Auth b) where
  had = had . authObject

makeAuth :: IdentityM m => a -> m (Auth a)
makeAuth o = Auth o `liftM` pull
