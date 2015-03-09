{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.SlotAsset.SQL
  ( selectContainerSlotAsset
  , selectAssetSlotAsset
  , selectVolumeSlotAsset
  , selectSlotAsset
  , selectAssetOrSlotAsset
  , insertSlotAsset
  , updateSlotAsset
  , deleteSlotAsset
  ) where

import qualified Language.Haskell.TH as TH

import Control.Has (view)
import Databrary.Model.Time.Types
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
makeContainerSlotAsset f af c = f (af (view c)) c

selectContainerSlotAsset :: Selector -- ^ @'Container' -> 'SlotAsset'@
selectContainerSlotAsset = selectJoin 'makeContainerSlotAsset
  [ selectAssetContainerSlotAsset
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset -- XXX volumes match?
  ]

makeAssetSlotAsset :: (Asset -> Container -> SlotAsset) -> (Volume -> Container) -> Asset -> SlotAsset
makeAssetSlotAsset f cf a = f a (cf (view a))

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

makeVolumeAssetSlot :: (Volume -> Asset) -> Maybe (Asset -> SlotAsset) -> Volume -> Either Asest SlotAsset
makeVolumeAssetSlot af sf v = maybe Left (Right .) sf $ af v

selectAssetOrSlotAsset :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Either Asset SlotAsset'@
selectAssetOrSlotAsset ident = selectJoin 'makeVolumeAssetSlot
  [ selectVolumeAsset
  , maybeJoinOn "asset.id = slot_asset.asset AND asset.volume = container.volume"
    selectAssetSlotAsset
  , joinOn "asset.volume = volume.id AND container.volume = volume.id"
    $ selectVolume ident
  ]

slotAssetKeys :: String -- ^ @'SlotAsset'@
  -> [(String, String)]
slotAssetKeys sa =
  [ ("asset", "${assetId (slotAsset " ++ sa ++ ")}") ]

slotAssetSets :: String -- ^ @'SlotAsset'@
  -> [(String, String)]
slotAssetSets sa =
  [ ("container", "${containerId (slotContainer (assetSlot " ++ sa ++ "))}")
  , ("segment", "${slotSegment (assetSlot " ++ sa ++ ")}")
  ]

insertSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'SlotAsset'@
  -> TH.ExpQ
insertSlotAsset ident a = auditInsert ident "slot_asset"
  (slotAssetKeys as ++ slotAssetSets as)
  Nothing
  where as = nameRef a

updateSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'SlotAsset'@
  -> TH.ExpQ
updateSlotAsset ident a = auditUpdate ident "slot_asset"
  (slotAssetSets as)
  (whereEq $ slotAssetKeys as)
  Nothing
  where as = nameRef a

deleteSlotAsset :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'SlotAsset'@
  -> TH.ExpQ
deleteSlotAsset ident a = auditDelete ident "slot_asset"
  (whereEq $ slotAssetKeys as)
  Nothing
  where as = nameRef a
