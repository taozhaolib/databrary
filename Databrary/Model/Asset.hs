{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Model.Asset
  ( module Databrary.Model.Asset.Types
  , assetJSON
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Monad (guard)
import Data.Maybe (catMaybes, isNothing)

import qualified Databrary.JSON as JSON
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types

assetJSON :: Asset -> JSON.Object
assetJSON Asset{..} = JSON.record assetId $ catMaybes
  [ Just $ "format" JSON..= formatId assetFormat
  , Just $ "classification" JSON..= assetClassification
  , ("name" JSON..=) <$> assetName
  , ("duration" JSON..=) <$> assetDuration
  , ("pending" JSON..= True) <$ guard (isNothing assetSHA1)
  ]
