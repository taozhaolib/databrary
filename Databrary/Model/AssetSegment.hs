{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.AssetSegment
  ( module Databrary.Model.AssetSegment.Types
  , lookupAssetSegment
  , lookupAssetSlotSegment
  ) where

import Databrary.Ops
import Databrary.Has (peek, view)
import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Segment
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment.Types
import Databrary.Model.AssetSegment.SQL

lookupAssetSegment :: (MonadHasIdentity c m, DBM m) => Id Asset -> Segment -> m (Maybe AssetSegment)
lookupAssetSegment ai seg = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) "$WHERE slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")

lookupAssetSlotSegment :: DBM m => AssetSlot -> Segment -> m (Maybe AssetSegment)
lookupAssetSlotSegment a s =
  if segmentEmpty seg
    then return Nothing
    else Just . as <$>
      dbQuery1 $(selectQuery excerptRow "$WHERE asset = ${view a :: Id Asset} AND segment @> ${seg}")
  where
  as = makeExcerpt a s
  seg = assetSegment $ as Nothing
