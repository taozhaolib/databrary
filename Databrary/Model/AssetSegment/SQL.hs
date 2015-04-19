{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.AssetSegment.SQL
  ( excerptRow
  , makeExcerpt
  , selectAssetSegment
  , selectContainerAssetSegment
  , selectAssetAssetSegment
  ) where

import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Databrary.Has (view)
import Databrary.Model.SQL.Select
import Databrary.Model.Permission.Types
import Databrary.Model.Segment
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSlot.SQL
import Databrary.Model.AssetSegment.Types

excerptTuple :: Segment -> Classification -> (Segment, Classification)
excerptTuple = (,)

excerptRow :: Selector -- ^ @'Classification'@
excerptRow = selectColumns 'excerptTuple "excerpt" ["segment", "classification"]

makeExcerpt :: AssetSlot -> Segment -> Maybe (Segment, Classification) -> AssetSegment
makeExcerpt a s = newAssetSegment a s . fmap (uncurry $ newExcerpt a)

makeAssetSegment :: Segment -> Maybe Segment -> Maybe (Segment, Classification) -> Asset -> Container -> AssetSegment
makeAssetSegment as ss e a c = makeExcerpt sa ss' e where
  sa = makeSlotAsset a c as
  ss' = fromMaybe emptySegment ss -- should not happen

selectAssetContainerAssetSegment :: TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'Asset' -> 'Container' -> 'AssetSegment'@
selectAssetContainerAssetSegment seg = selectJoin 'makeAssetSegment
  [ slotAssetRow
  , crossJoin 
    $ selector ("LATERAL (VALUES (slot_asset.segment * ${" ++ nameRef seg ++ "})) AS asset_segment (segment)")
      "asset_segment.segment"
  , maybeJoinOn "slot_asset.asset = excerpt.asset AND asset_segment.segment <@ excerpt.segment"
    excerptRow
  ]

makeContainerAssetSegment :: (Asset -> Container -> AssetSegment) -> (Volume -> Asset) -> Container -> AssetSegment
makeContainerAssetSegment f af c = f (af (view c)) c

selectContainerAssetSegment :: TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'Container' -> 'AssetSegment'@
selectContainerAssetSegment seg = selectJoin 'makeContainerAssetSegment
  [ selectAssetContainerAssetSegment seg
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset -- XXX volumes match?
  ]

makeAssetAssetSegment :: (Asset -> Container -> AssetSegment) -> (Volume -> Container) -> Asset -> AssetSegment
makeAssetAssetSegment f cf a = f a (cf (view a))

selectAssetAssetSegment :: TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'Container' -> 'AssetSegment'@
selectAssetAssetSegment seg = selectJoin 'makeAssetAssetSegment
  [ selectAssetContainerAssetSegment seg
  , joinOn "slot_asset.container = container.id"
    selectVolumeContainer -- XXX volumes match?
  ]

makeVolumeAssetSegment :: (Asset -> Container -> AssetSegment) -> (Volume -> Asset) -> (Volume -> Container) -> Volume -> AssetSegment
makeVolumeAssetSegment f af cf v = f (af v) (cf v)

selectVolumeAssetSegment :: TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'Volume' -> 'AssetSegment'@
selectVolumeAssetSegment seg = selectJoin 'makeVolumeAssetSegment
  [ selectAssetContainerAssetSegment seg
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset
  , joinOn "slot_asset.container = container.id AND asset.volume = container.volume"
    selectVolumeContainer
  ]

selectAssetSegment :: TH.Name -- ^ @'Identity'@
  -> TH.Name -- ^ @'Segment'@
  -> Selector -- ^ @'AssetSegment'@
selectAssetSegment ident seg = selectJoin '($)
  [ selectVolumeAssetSegment seg
  , joinOn "asset.volume = volume.id"
    $ selectVolume ident
  ]
