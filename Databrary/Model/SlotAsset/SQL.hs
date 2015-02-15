{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.SlotAsset.SQL
  ( selectContainerSlotAsset
  , selectAssetSlotAsset
  , selectVolumeSlotAsset
  , selectSlotAsset
  ) where

import qualified Language.Haskell.TH as TH

import Control.Has (see)
import Databrary.Model.Time.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Slot.Types
import Databrary.Model.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.SlotAsset.Types

slotAssetRow :: Selector -- ^ @'Segment'@
slotAssetRow = selector "slot_asset" "slot_asset.segment"

excerptRow :: Selector -- ^ @'Classification'@
excerptRow = selector "excerpt" "excerpt.classification"

makeSlotAsset :: Segment -> Maybe Classification -> Asset -> Container -> SlotAsset
makeSlotAsset seg cls a c = SlotAsset a (Slot c seg) cls

selectAssetContainerSlotAsset :: Selector -- ^ @'Asset' -> 'Container' -> 'SlotAsset'@
selectAssetContainerSlotAsset = selectJoin 'makeSlotAsset
  [ slotAssetRow
  , maybeJoinOn "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment"
    excerptRow
  ]

makeContainerSlotAsset :: (Asset -> Container -> SlotAsset) -> (Volume -> Asset) -> Container -> SlotAsset
makeContainerSlotAsset f af c = f (af (see c)) c

selectContainerSlotAsset :: Selector -- ^ @'Container' -> 'SlotAsset'@
selectContainerSlotAsset = selectJoin 'makeContainerSlotAsset
  [ selectAssetContainerSlotAsset
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset -- XXX volumes match?
  ]

makeAssetSlotAsset :: (Asset -> Container -> SlotAsset) -> (Volume -> Container) -> Asset -> SlotAsset
makeAssetSlotAsset f cf a = f a (cf (see a))

selectAssetSlotAsset :: Selector -- ^ @'Asset' -> 'SlotAsset'@
selectAssetSlotAsset = selectJoin 'makeAssetSlotAsset
  [ selectAssetContainerSlotAsset
  , joinOn "slot_asset.container = container.id"
    selectVolumeContainer -- XXX volumes match?
  ]

makeVolumeSlotAsset :: (Asset -> Container -> SlotAsset) -> (Volume -> Asset) -> (Volume -> Container) -> Volume -> SlotAsset
makeVolumeSlotAsset f af cf v = f (af v) (cf v)

selectVolumeSlotAsset :: Selector -- ^ @'Volume' -> 'SlotAsset'@
selectVolumeSlotAsset = selectJoin 'makeVolumeSlotAsset
  [ selectAssetContainerSlotAsset
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset
  , joinOn "slot_asset.container = container.id AND asset.volume = container.volume"
    selectVolumeContainer
  ]

selectSlotAsset :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'SlotAsset'@
selectSlotAsset ident = selectJoin 'makeVolumeSlotAsset
  [ selectAssetContainerSlotAsset
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset
  , joinOn "slot_asset.container = container.id"
    selectVolumeContainer
  , joinOn "asset.volume = volume.id AND container.volume = volume.id"
    $ selectVolume ident
  ]
