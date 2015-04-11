{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.AssetSegment.SQL
  ( selectAssetSegment
  ) where

import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Permission.Types
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.AssetSlot.SQL
import Databrary.Model.AssetSegment.Types

makeAssetSegment :: Segment -> Maybe Segment -> Maybe Classification -> (Volume -> Asset) -> (Volume -> Container) -> Volume -> AssetSegment
makeAssetSegment as ss ec a c v = AssetSegment (makeSlotAsset as (guard (ss' == as) >> ec) (a v) (c v)) ss' ec
  where ss' = fromMaybe emptySegment ss -- should not happen

selectAssetSegment :: TH.Name -- ^ @'Identity'@
  -> TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'AssetSegment'@
selectAssetSegment ident seg = selectJoin 'makeAssetSegment
  [ slotAssetRow
  , crossJoin 
    $ selector ("LATERAL (VALUES (slot_asset.segment * ${" ++ nameRef seg ++ "})) AS asset_segment (segment)")
      "asset_segment.segment"
  , maybeJoinOn "slot_asset.asset = excerpt.asset AND asset_segment.segment <@ excerpt.segment"
    excerptRow
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset
  , joinOn "slot_asset.container = container.id"
    selectVolumeContainer
  , joinOn "asset.volume = volume.id AND container.volume = volume.id"
    $ selectVolume ident
  ]
