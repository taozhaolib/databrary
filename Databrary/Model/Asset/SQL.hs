{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Asset.SQL
  ( selectVolumeAsset
  , selectAsset
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH

import Databrary.Model.Time.Types
import Databrary.Model.Format
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.SQL.Select
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Asset.Types

makeAsset :: Id Asset -> Id Format -> Classification -> Maybe T.Text -> Maybe Offset -> Maybe BS.ByteString -> Volume -> Asset
makeAsset i = Asset i . getFormat'

assetRow :: Selector -- ^ @'Volume' -> 'Asset'@
assetRow = selectColumns 'makeAsset "asset" ["id", "format", "classification", "name", "duration", "sha1"]

selectVolumeAsset :: Selector -- ^ @'Volume' -> 'Container'@
selectVolumeAsset = assetRow

selectAsset :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Container'@
selectAsset ident = selectJoin '($)
  [ selectVolumeAsset
  , joinOn "asset.volume = volume.id" $ selectVolume ident
  ]
