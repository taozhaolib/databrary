{-# LANGUAGE TemplateHaskell #-}
module Databrary.Identity.Types
  ( Identity(..)
  , identityId
  , IdentityM
  ) where

import Control.Has (HasM, makeHasFor, see)
import Databrary.Model.Authorize.Types
import Databrary.Model.Party.Types
import Databrary.Model.Id.Types

data Identity = Identity
  { identityAuthorization :: PartyAuth
  , identitySuperuser :: Bool
  }

makeHasFor ''Identity
  [ ('identityAuthorization, [''Authorization, ''Party, ''Access])
  ]

identityId :: Identity -> Id Party
identityId = partyId . see

type IdentityM c m = HasM Identity c m
