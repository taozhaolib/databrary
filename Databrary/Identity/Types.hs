{-# LANGUAGE TemplateHaskell #-}
module Databrary.Identity.Types
  ( Identity(..)
  , identityId
  , IdentityM
  ) where

import Control.Has (makeHasRec, see)
import Databrary.Model.Authorize.Types
import Databrary.Model.Party.Types
import Databrary.Model.Id.Types

data Identity = Identity
  { identityAuthorization :: PartyAuth
  , identitySuperuser :: Bool
  }

makeHasRec ''Identity ['identityAuthorization]

identityId :: Identity -> Id Party
identityId = partyId . see

type IdentityM c m = MonadHasIdentity c m
