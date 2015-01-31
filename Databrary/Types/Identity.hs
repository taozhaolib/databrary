{-# LANGUAGE TemplateHaskell #-}
module Databrary.Types.Identity
  ( Identity(..)
  , identityId
  , IdentityM
  ) where

import Control.Has (HasM, makeHasFor, see)
import Databrary.Model.Types.Authorize
import Databrary.Model.Types.Party
import Databrary.Model.Types.Id

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
