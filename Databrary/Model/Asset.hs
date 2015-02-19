{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Model.Asset
  ( module Databrary.Model.Asset.Types
  , assetJSON
  ) where

import Data.Maybe (catMaybes, isNothing)

import Control.Applicative.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.Time ()
import Databrary.Model.Format.Types
import Databrary.Model.Asset.Types

assetJSON :: Asset -> JSON.Object
assetJSON Asset{..} = JSON.record assetId $ catMaybes
  [ Just $ "format" JSON..= formatId assetFormat
  , Just $ "classification" JSON..= assetClassification
  , ("name" JSON..=) <$> assetName
  , ("duration" JSON..=) <$> assetDuration
  , ("pending" JSON..= True) <? isNothing assetSHA1
  ]
