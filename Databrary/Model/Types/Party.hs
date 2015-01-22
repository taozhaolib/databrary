{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Types.Party 
  ( Party(..)
  , Account(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T

import Control.Monad.Has (Has(..))
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

instance HasId Party where
  idOf = partyId
  kindOf _ = "party"

instance Has Party Account where
  had = accountParty
