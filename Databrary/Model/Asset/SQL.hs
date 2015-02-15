{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Asset.SQL
  ( selectVolumeAsset
  , selectAsset
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.Format
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
