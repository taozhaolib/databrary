{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Asset.Types
  ( Asset(..)
  , MonadHasAsset
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int64)
import qualified Data.Text as T

import Control.Has (makeHasRec)
import Databrary.Model.Time.Types
import Databrary.Model.Kind
import Databrary.Model.Permission.Types
import Databrary.Model.Id.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Format.Types

type instance IdType Asset = Int32

data Asset = Asset
  { assetId :: Id Asset
  , assetFormat :: Format
  , assetClassification :: Classification
  , assetName :: Maybe T.Text
  , assetDuration :: Maybe Offset
  , assetSHA1 :: Maybe BS.ByteString
  , assetSize :: Maybe Int64
  , assetVolume :: Volume
  }

instance Kinded Asset where
  kindOf _ = "asset"

makeHasRec ''Asset ['assetId, 'assetFormat, 'assetClassification, 'assetVolume]
