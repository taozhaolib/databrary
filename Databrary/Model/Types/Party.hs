{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Types.Party 
  ( Party(..)
  , Account(..)
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Monad (guard)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes, isNothing)
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

instance JSON.ToJSON Party where
  toJSON p = JSON.object $ catMaybes
    [ Just $ "name" JSON..= partyName p
    , ("affiliation" JSON..=) <$> partyAffiliation p
    , ("url" JSON..=) <$> partyURL p
    , "institution" JSON..= True <$ guard (isNothing a)
    , ("email" JSON..=) . accountEmail <$> a -- TODO: permissions
    ] where a = partyAccount p
