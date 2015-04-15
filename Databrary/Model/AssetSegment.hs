{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.AssetSegment
  ( module Databrary.Model.AssetSegment.Types
  , lookupAssetSegment
  , lookupSlotAssetSegment
  , lookupAssetSlotSegment
  , assetSegmentJSON
  ) where

import qualified Data.Foldable as Fold
import Data.Maybe (catMaybes)

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Segment
import Databrary.Model.Slot
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment.Types
import Databrary.Model.AssetSegment.SQL

lookupAssetSegment :: (MonadHasIdentity c m, DBM m) => Segment -> Id Asset -> m (Maybe AssetSegment)
lookupAssetSegment seg ai = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) "$WHERE slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")

lookupSlotAssetSegment :: (MonadHasIdentity c m, DBM m) => Id Slot -> Id Asset -> m (Maybe AssetSegment)
lookupSlotAssetSegment (Id (SlotId ci seg)) ai = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) "$WHERE slot_asset.container = ${ci} AND slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")

lookupAssetSlotSegment :: DBM m => AssetSlot -> Segment -> m (Maybe AssetSegment)
lookupAssetSlotSegment a s =
  if segmentEmpty seg
    then return Nothing
    else Just . as <$>
      dbQuery1 $(selectQuery excerptRow "$WHERE asset = ${view a :: Id Asset} AND segment @> ${seg}")
  where
  as = makeExcerpt a s
  seg = assetSegment $ as Nothing

assetSegmentJSON :: AssetSegment -> JSON.Object
assetSegmentJSON AssetSegment{..} = assetSlotJSON segmentAsset JSON..++ catMaybes
  [ Fold.any ((assetSegment ==) . slotSegment) (assetSlot segmentAsset) ?!> ("segment" JSON..= assetSegment)
  , ("excerpt" JSON..=) . excerptClassification <$> assetExcerpt
  ]
