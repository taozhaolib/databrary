{-# LANGUAGE TemplateHaskell #-}
module Databrary.Identity.Types
  ( Identity(..)
  , MonadHasIdentity
  ) where

import Control.Has (makeHasRec)
import Databrary.Model.Authorize.Types

data Identity = Identity
  { identityAuthorization :: PartyAuth
  , identitySuperuser :: Bool
  }

makeHasRec ''Identity ['identityAuthorization]
