{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Authorize.Types
  ( Authorization(..)
  , MonadHasAuthorization
  , Authorize(..)
  , MonadHasAuthorize
  , AuthParty(..)
  , MonadHasAuthParty
  , PartyAuth(..)
  , MonadHasPartyAuth
  ) where

import Data.Time (UTCTime)

import Control.Has (Has(..), makeHasRec)
import Databrary.Model.Permission.Types
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types

data Authorization = Authorization
  { authorizeAccess :: !Access
  , authorizeChild :: Party
  , authorizeParent :: Party
  }

makeHasRec ''Authorization ['authorizeAccess]

data Authorize = Authorize
  { authorization :: Authorization
  , authorizeExpires :: UTCTime
  }

makeHasRec ''Authorize ['authorization]

-- |'Authorization' representing (access to) the parent
newtype AuthParty = AuthParty { authPartyAuthorization :: Authorization }

makeHasRec ''AuthParty ['authPartyAuthorization]

instance Has Party AuthParty where
  view f (AuthParty (Authorization a c p)) =
    fmap (\p' -> AuthParty (Authorization a c p')) (f p)
  see (AuthParty a) = authorizeParent a
instance Has (Id Party) AuthParty where
  view = view . partyId'
  see = partyId . see

-- |'Authorization' representing the child('s access)
newtype PartyAuth = PartyAuth { partyAuthAuthorization :: Authorization }

makeHasRec ''PartyAuth ['partyAuthAuthorization]

instance Has Party PartyAuth where
  view f (PartyAuth (Authorization a c p)) =
    fmap (\c' -> PartyAuth (Authorization a c' p)) (f c)
  see (PartyAuth a) = authorizeChild a
instance Has (Id Party) PartyAuth where
  view = view . partyId'
  see = partyId . see

