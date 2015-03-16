{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Excerpt
  ( module Databrary.Model.Excerpt.Types
  , lookupAssetExcerpts
  , changeExcerpt
  , removeExcerpt
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
import Databrary.Model.Excerpt.Types
import Databrary.Model.Excerpt.SQL

lookupAssetExcerpts :: DBM m => AssetSlot -> m [Excerpt]
lookupAssetExcerpts a =
  dbQuery $ ($ a) <$> $(selectQuery selectAssetSlotExcerpt "$WHERE excerpt.asset = ${assetId $ slotAsset a}")

changeExcerpt :: MonadAudit c m => Excerpt -> m Bool
changeExcerpt e = do
  ident <- getAuditIdentity
  either (const False) ((0 <) . fst) <$> tryUpdateOrInsert (guard . isExclusionViolation)
    $(updateExcerpt 'ident 'e)
    $(insertExcerpt 'ident 'e)

removeExcerpt :: MonadAudit c m => Excerpt -> m Bool
removeExcerpt e = do
  ident <- getAuditIdentity
  (0 <) <$> dbExecute $(deleteExcerpt 'ident 'e)

excerptJSON :: Excerpt -> JSON.Object
excerptJSON e@Excerpt{..} = JSON.object $ catMaybes
  [ segmentFull excerptSegment ?!> "segment" JSON..= excerptSegment
  , Just $ "classification" JSON..= excerptClassification
  , Just $ "permission" JSON..= (view e :: Permission)
  ]
