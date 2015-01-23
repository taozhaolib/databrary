{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}
module Databrary.Types.Identity
  ( Identity(..)
  , IdentityM
  ) where

import Control.Has (HasM, makeHasFor)
import Databrary.Model.Types.Authorize

data Identity = Identity
  { identityAuthorization :: Authorization
  , identitySuperuser :: Bool
  }

makeHasFor 
  [ ('identityAuthorization, [''Access])
  ] ''Identity

type IdentityM c m = HasM Identity c m
