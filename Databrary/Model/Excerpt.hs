{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Excerpt
  ( lookupAssetExcerpts
  , changeExcerpt
  , removeExcerpt
  ) where

import Control.Monad (guard)

import Databrary.Ops
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Asset.Types
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSegment.Types
import Databrary.Model.Excerpt.SQL

lookupAssetExcerpts :: MonadDB m => AssetSlot -> m [Excerpt]
lookupAssetExcerpts a =
  dbQuery $ ($ a) <$> $(selectQuery selectAssetSlotExcerpt "$WHERE excerpt.asset = ${assetId $ slotAsset a}")

changeExcerpt :: MonadAudit c m => Excerpt -> m Bool
changeExcerpt e = do
  ident <- getAuditIdentity
  either (const False) ((0 <) . fst) <$> tryUpdateOrInsert (guard . isExclusionViolation)
    $(updateExcerpt 'ident 'e)
    $(insertExcerpt 'ident 'e)

removeExcerpt :: MonadAudit c m => AssetSegment -> m Bool
removeExcerpt e = do
  ident <- getAuditIdentity
  dbExecute1 $(deleteExcerpt 'ident 'e)
