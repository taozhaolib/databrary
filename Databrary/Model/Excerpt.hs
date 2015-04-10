{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Excerpt
  ( lookupAssetExcerpts
  , changeExcerpt
  , excerptJSON
  ) where

import Control.Monad (guard)
import Data.Maybe (catMaybes)

import Databrary.Ops
import Databrary.Has (view)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Segment
import Databrary.Model.Audit
import Databrary.Model.Permission
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Model.Excerpt.SQL

lookupAssetExcerpts :: DBM m => AssetSlot -> m [AssetSegment]
lookupAssetExcerpts a =
  dbQuery $ ($ a) <$> $(selectQuery selectAssetSlotExcerpt "$WHERE excerpt.asset = ${assetId $ slotAsset a}")

changeExcerpt :: MonadAudit c m => AssetSegment -> m Bool
changeExcerpt e = do
  ident <- getAuditIdentity
  case assetSegmentExcerpt e of
    Just _ ->
      either (const False) ((0 <) . fst) <$> tryUpdateOrInsert (guard . isExclusionViolation)
        $(updateExcerpt 'ident 'e)
        $(insertExcerpt 'ident 'e)
    Nothing -> dbExecute1 $(deleteExcerpt 'ident 'e)

excerptJSON :: AssetSegment -> JSON.Object
excerptJSON e@AssetSegment{..} = JSON.object $ catMaybes
  [ segmentFull assetSegment ?!> "segment" JSON..= assetSegment
  , ("classification" JSON..=) <$> assetSegmentExcerpt
  , Just $ "permission" JSON..= (view e :: Permission)
  ]
