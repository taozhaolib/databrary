{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.AssetSegment
  ( module Databrary.Model.AssetSegment.Types
  , lookupAssetSegment
  , lookupSlotAssetSegment
  , lookupAssetSlotSegment
  , auditAssetSegmentDownload
  , assetSegmentJSON
  ) where

import Data.Maybe (catMaybes)
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Model.Segment
import Databrary.Model.Volume
import Databrary.Model.Container
import Databrary.Model.Slot
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment.Types
import Databrary.Model.AssetSegment.SQL

lookupAssetSegment :: (MonadHasIdentity c m, MonadDB m) => Segment -> Id Asset -> m (Maybe AssetSegment)
lookupAssetSegment seg ai = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) "$WHERE slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")

lookupSlotAssetSegment :: (MonadHasIdentity c m, MonadDB m) => Id Slot -> Id Asset -> m (Maybe AssetSegment)
lookupSlotAssetSegment (Id (SlotId ci seg)) ai = do
  ident :: Identity <- peek
  dbQuery1 $(selectQuery (selectAssetSegment 'ident 'seg) "$WHERE slot_asset.container = ${ci} AND slot_asset.asset = ${ai} AND slot_asset.segment && ${seg}")

lookupAssetSlotSegment :: MonadDB m => AssetSlot -> Segment -> m (Maybe AssetSegment)
lookupAssetSlotSegment a s =
  if segmentEmpty seg
    then return Nothing
    else Just . as <$>
      dbQuery1 $(selectQuery excerptRow "$WHERE asset = ${view a :: Id Asset} AND segment @> ${seg}")
  where
  as = makeExcerpt a s
  seg = assetSegment $ as Nothing

auditAssetSegmentDownload :: MonadAudit c m => Bool -> AssetSegment -> m ()
auditAssetSegmentDownload success AssetSegment{ segmentAsset = AssetSlot{ slotAsset = a, assetSlot = as }, assetSegment = seg } = do
  ai <- getAuditIdentity
  maybe
    (dbExecute1' [pgSQL|INSERT INTO audit.asset (audit_action, audit_user, audit_ip, id, volume, format, classification) VALUES
      (${act}, ${auditWho ai}, ${auditIp ai}, ${assetId a}, ${volumeId $ assetVolume a}, ${formatId $ assetFormat a}, ${assetClassification a})|])
    (\s -> dbExecute1' [pgSQL|$INSERT INTO audit.slot_asset (audit_action, audit_user, audit_ip, container, segment, asset) VALUES
      (${act}, ${auditWho ai}, ${auditIp ai}, ${containerId $ slotContainer s}, ${seg}, ${assetId a})|])
    as
  where act | success = AuditActionOpen 
            | otherwise = AuditActionAttempt

assetSegmentJSON :: AssetSegment -> JSON.Object
assetSegmentJSON as@AssetSegment{..} = assetSlotJSON segmentAsset JSON..++ catMaybes
  -- conflicts with asset...
  [ assetSegmentFull as ?!> ("segment" JSON..= assetSegment)
  , view segmentAsset == fmt ?!> "format" JSON..= formatId fmt
  , ("excerpt" JSON..=) . excerptClassification <$> assetExcerpt
  -- , context? excerpt segment?
  ] where fmt = view as
