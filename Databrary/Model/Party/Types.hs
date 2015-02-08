{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Party.Types 
  ( Party(..)
  , partyId'
  , MonadHasParty
  , Account(..)
  , MonadHasAccount
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)

import Control.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types

type instance IdType Party = Int32

data Party = Party
  { partyId :: Id Party
  , partyName :: T.Text
  , partyAffiliation :: Maybe T.Text
  , partyURL :: Maybe T.Text
  , partyAccount :: Maybe Account
  }

data Account = Account
  { accountEmail :: T.Text
  , accountPasswd :: Maybe BS.ByteString
  , accountParty :: Party
  }

instance Kinded Party where
  kindOf _ = "party"

makeHasRec ''Party ['partyId]
makeHasRec ''Account ['accountParty]
deriveLiftMany [''Party, ''Account]
