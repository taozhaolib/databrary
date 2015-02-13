{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Party.Types 
  ( Party(..)
  , partyId'
  , nobodyParty
  , MonadHasParty
  , Account(..)
  , MonadHasAccount
  , SiteAuth(..)
  , MonadHasSiteAuth
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Instances.TH.Lift ()
import Language.Haskell.TH.Lift (deriveLiftMany)

import Control.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types

type instance IdType Party = Int32

data Party = Party
  { partyId :: Id Party
  , partyName :: T.Text
  , partyAffiliation :: Maybe T.Text
  , partyURL :: Maybe T.Text
  , partyAccount :: Maybe Account
  , partyPermission :: Permission
  }

data Account = Account
  { accountEmail :: T.Text
  , accountPasswd :: Maybe BS.ByteString
  , accountParty :: Party
  }

instance Kinded Party where
  kindOf _ = "party"

data SiteAuth = SiteAuth
  { siteAccount :: Account -- maybe should be Party (for nobody)
  , siteAccess :: Access
  }

-- this is unfortunate, mainly to avoid untangling Party.SQL
nobodyParty :: Party
nobodyParty = Party
  { partyId = Id (-1)
  , partyName = "Nobody"
  , partyAffiliation = Nothing
  , partyURL = Nothing
  , partyAccount = Nothing
  , partyPermission = PermissionREAD
  }

makeHasRec ''Party ['partyId, 'partyPermission]
makeHasRec ''Account ['accountParty]
makeHasRec ''SiteAuth ['siteAccount, 'siteAccess]
deriveLiftMany [''Party, ''Account]
