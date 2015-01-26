{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Types.Party 
  ( Party(..)
  , Account(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Control.Has (Has(..))
import Databrary.Model.Types.Id

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

instance Has (Id Party) Party where
  view f p = fmap (\i -> p{ partyId = i }) $ f $ partyId p
  see = partyId
instance HasId Party where
  kindOf _ = "party"

instance Has Party Account where
  view f a = fmap (\p -> a{ accountParty = p }) $ f $ accountParty a
  see = accountParty
instance Has (Id Party) Account where
  view f a = fmap (\i -> a{ accountParty = (accountParty a){ partyId = i } }) $ f $ partyId $ accountParty a
  see = partyId . accountParty
