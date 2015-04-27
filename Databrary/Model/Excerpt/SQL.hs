{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Excerpt.SQL
  ( selectAssetSlotExcerpt
  , selectContainerExcerpt
  , selectVolumeExcerpt
  , insertExcerpt
  , updateExcerpt
  , deleteExcerpt
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Release.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Container.Types
import Databrary.Model.Container.SQL
import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Asset.SQL
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSlot.SQL
import Databrary.Model.AssetSegment.Types

makeExcerpt :: Segment -> Maybe Release -> AssetSlot -> Excerpt
makeExcerpt s r a = newExcerpt a s r

excerptRow :: Selector -- ^ @'AssetSlot' -> 'Excerpt'@
excerptRow = selectColumns 'makeExcerpt "excerpt" ["segment", "release"]

selectAssetSlotExcerpt :: Selector -- ^ @'AssetSlot' -> 'Excerpt'@
selectAssetSlotExcerpt = excerptRow

makeAssetContainerExcerpt :: Segment -> (AssetSlot -> Excerpt) -> Asset -> Container -> Excerpt
makeAssetContainerExcerpt as e a c = e $ makeSlotAsset a c as

selectAssetContainerExcerpt :: Selector -- ^ @'Asset' -> 'Container' -> 'Excerpt'@
selectAssetContainerExcerpt = selectJoin 'makeAssetContainerExcerpt
  [ slotAssetRow
  , joinOn "slot_asset.asset = excerpt.asset"
    excerptRow
  ]

makeContainerExcerpt :: (Asset -> Container -> Excerpt) -> (Volume -> Asset) -> Container -> Excerpt
makeContainerExcerpt f af c = f (af (containerVolume c)) c

selectContainerExcerpt :: Selector -- ^ @'Container' -> 'Excerpt'@
selectContainerExcerpt = selectJoin 'makeContainerExcerpt
  [ selectAssetContainerExcerpt
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset -- XXX volumes match?
  ]

makeVolumeExcerpt :: (Asset -> Container -> Excerpt) -> (Volume -> Asset) -> (Volume -> Container) -> Volume -> Excerpt
makeVolumeExcerpt f af cf v = f (af v) (cf v)

selectVolumeExcerpt :: Selector -- ^ @'Volume' -> 'Excerpt'@
selectVolumeExcerpt = selectJoin 'makeVolumeExcerpt
  [ selectAssetContainerExcerpt
  , joinOn "slot_asset.asset = asset.id"
    selectVolumeAsset
  , joinOn "slot_asset.container = container.id AND asset.volume = container.volume"
    selectVolumeContainer
  ]

excerptKeys :: String -- ^ @'Excerpt'@
  -> [(String, String)]
excerptKeys o =
  [ ("asset", "${assetId $ slotAsset $ segmentAsset $ excerptAsset " ++ o ++ "}")
  , ("segment", "${assetSegment $ excerptAsset " ++ o ++ "}")
  ]

excerptSets :: String -- ^ @'Excerpt'@
  -> [(String, String)]
excerptSets o =
  [ ("release", "${excerptRelease " ++ o ++ "}")
  ]

insertExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Excerpt'@
  -> TH.ExpQ
insertExcerpt ident o = auditInsert ident "excerpt"
  (excerptKeys os ++ excerptSets os)
  Nothing
  where os = nameRef o

updateExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Excerpt'@
  -> TH.ExpQ
updateExcerpt ident o = auditUpdate ident "excerpt"
  (excerptSets os)
  (whereEq $ excerptKeys os)
  Nothing
  where os = nameRef o

deleteExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSegment'@
  -> TH.ExpQ
deleteExcerpt ident o = auditDelete ident "excerpt"
  ("asset = ${assetId $ slotAsset $ segmentAsset " ++ os ++ "} AND segment <@ ${assetSegment " ++ os ++ "}")
  Nothing
  where os = nameRef o
