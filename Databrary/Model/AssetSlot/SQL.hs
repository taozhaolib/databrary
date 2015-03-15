{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.AssetSlot.SQL
  ( selectContainerSlotAsset
  , selectAssetSlotAsset
  , selectVolumeSlotAsset
  , selectSlotAsset
  , selectVolumeAssetSlot
  , selectAssetSlot
  , insertSlotAsset
  , updateSlotAsset
  , deleteSlotAsset
  ) where

import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Control.Has (view)
import Databrary.Model.Segment
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Slot.Types
import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Volume.SQL
import Databrary.Model.AssetSlot.Types

slotAssetRow :: Selector -- ^ @'Segment'@
slotAssetRow = selector "slot_asset" "slot_asset.segment"

excerptRow :: Selector -- ^ @'Classification'@
excerptRow = selector "excerpt" "excerpt.classification"

makeSlotAsset :: Segment -> Maybe Classification -> Asset -> Container -> AssetSlot
makeSlotAsset seg cls a c = AssetSlot a (Just (Slot c seg)) cls

selectAssetContainerSlotAsset :: Selector -- ^ @'Asset' -> 'Container' -> 'AssetSlot'@
selectAssetContainerSlotAsset = selectJoin 'makeSlotAsset
  [ slotAssetRow
  , maybeJoinOn "slot_asset.asset = excerpt.asset AND slot_asset.segment <@ excerpt.segment"
    excerptRow
  ]

makeContainerSlotAsset :: (Asset -> Container -> AssetSlot) -> (Volume -> Asset) -> Container -> AssetSlot
makeContainerSlotAsset f af c = f (af (view c)) c

selectContainerSlotAsset :: Selector -- ^ @'Container' -> 'AssetSlot'@
selectContainerSlotAsset = selectJoin 'makeContainerSlotAsset
  [ selectAssetContainerSlotAsset
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset -- XXX volumes match?
  ]

makeAssetSlotAsset :: (Asset -> Container -> AssetSlot) -> (Volume -> Container) -> Asset -> AssetSlot
makeAssetSlotAsset f cf a = f a (cf (view a))

selectAssetSlotAsset :: Selector -- ^ @'Asset' -> 'AssetSlot'@
selectAssetSlotAsset = selectJoin 'makeAssetSlotAsset
  [ selectAssetContainerSlotAsset
  , joinOn "slot_asset.container = container.id"
    selectVolumeContainer -- XXX volumes match?
  ]

makeVolumeSlotAsset :: (Asset -> Container -> AssetSlot) -> (Volume -> Asset) -> (Volume -> Container) -> Volume -> AssetSlot
makeVolumeSlotAsset f af cf v = f (af v) (cf v)

selectVolumeSlotAsset :: Selector -- ^ @'Volume' -> 'AssetSlot'@
selectVolumeSlotAsset = selectJoin 'makeVolumeSlotAsset
  [ selectAssetContainerSlotAsset
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset
  , joinOn "slot_asset.container = container.id AND asset.volume = container.volume"
    selectVolumeContainer
  ]

selectSlotAsset :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'AssetSlot'@
selectSlotAsset ident = selectJoin '($)
  [ selectVolumeSlotAsset
  , joinOn "asset.volume = volume.id"
    $ selectVolume ident
  ]

makeVolumeAssetSlot :: (Volume -> Asset) -> Maybe (Asset -> AssetSlot) -> Volume -> AssetSlot
makeVolumeAssetSlot af sf = fromMaybe assetNoSlot sf . af

selectVolumeAssetSlot :: Selector -- ^ @'Volume' -> 'AssetSlot'@
selectVolumeAssetSlot = selectJoin 'makeVolumeAssetSlot
  [ selectVolumeAsset
  , maybeJoinOn "asset.id = slot_asset.asset AND asset.volume = container.volume"
    selectAssetSlotAsset
  ]

selectAssetSlot :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'AssetSlot'@
selectAssetSlot ident = selectJoin '($)
  [ selectVolumeAssetSlot
  , joinOn "asset.volume = volume.id"
    $ selectVolume ident
  ]

slotAssetKeys :: String -- ^ @'AssetSlot'@
  -> [(String, String)]
slotAssetKeys as =
  [ ("asset", "${assetId (slotAsset " ++ as ++ ")}") ]

slotAssetSets :: String -- ^ @'AssetSlot'@
  -> [(String, String)]
slotAssetSets as =
  [ ("container", "${containerId . slotContainer <$> assetSlot " ++ as ++ "}")
  , ("segment", "${slotSegment <$> assetSlot " ++ as ++ "}")
  ]

insertSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSlot'@
  -> TH.ExpQ
insertSlotAsset ident o = auditInsert ident "slot_asset"
  (slotAssetKeys os ++ slotAssetSets os)
  Nothing
  where os = nameRef o

updateSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSlot'@
  -> TH.ExpQ
updateSlotAsset ident o = auditUpdate ident "slot_asset"
  (slotAssetSets os)
  (whereEq $ slotAssetKeys os)
  Nothing
  where os = nameRef o

deleteSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSlot'@
  -> TH.ExpQ
deleteSlotAsset ident o = auditDelete ident "slot_asset"
  (whereEq $ slotAssetKeys os)
  Nothing
  where os = nameRef o
