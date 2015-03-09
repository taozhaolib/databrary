{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.SlotAsset
  ( module Databrary.Model.SlotAsset.Types
  , lookupSlotAsset
  , lookupAssetOrSlotAsset
  , lookupAssetSlotAsset
  , changeSlotAsset
  , removeSlotAsset
  , auditSlotAssetDownload
  , slotAssetJSON
  ) where

import Control.Monad (when)
import Data.Maybe (catMaybes)
import Database.PostgreSQL.Typed (pgSQL)
import qualified Database.PostgreSQL.Typed.Range as Range

import Control.Applicative.Ops
import Control.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.Time
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Asset
import Databrary.Model.Audit
import Databrary.Model.SQL
import Databrary.Model.SlotAsset.Types
import Databrary.Model.SlotAsset.SQL

lookupSlotAsset :: (MonadHasIdentity c m, DBM m) => Id Asset -> m (Maybe SlotAsset)
lookupSlotAsset ai = do
  ident <- peek
  dbQuery1 $(selectQuery (selectSlotAsset 'ident) "$WHERE asset.id = ${ai}")

lookupAssetOrSlotAsset :: (MonadHasIdentity c m, DBM m) => Id Asset -> m (Maybe (Either Asset SlotAsset))
lookupAssetOrSlotAsset ai = do
  ident <- peek
  dbQuery1 $(selectQuery (selectAssetOrSlotAsset 'ident) "$WHERE asset.id = ${ai}")

lookupAssetSlotAsset :: (DBM m) => Asset -> m (Maybe SlotAsset)
lookupAssetSlotAsset a =
  dbQuery1 $ ($ a) <$> $(selectQuery selectAssetSlotAsset "$WHERE asset.id = ${assetId a}")

changeSlotAsset :: (MonadAudit c m) => SlotAsset -> m ()
changeSlotAsset sa = do
  ident <- getAuditIdentity
  (r, _) <- updateOrInsert
    $(updateSlotAsset 'ident 'sa)
    $(insertSlotAsset 'ident 'sa)
  when (r /= 1) $ fail $ "changeSlotAsset: " ++ show r ++ " rows"

removeSlotAsset :: (MonadAudit c m) => SlotAsset -> m Bool
removeSlotAsset auth = do
  ident <- getAuditIdentity
  (0 <) <$> dbExecute $(deleteSlotAsset 'ident 'auth)

auditSlotAssetDownload :: MonadAudit c m => Bool -> SlotAsset -> m ()
auditSlotAssetDownload success sa = do
  ai <- getAuditIdentity
  dbExecute1 [pgSQL|INSERT INTO audit.slot_asset (audit_action, audit_user, audit_ip, container, segment, asset) VALUES
    (${if success then AuditActionOpen else AuditActionAttempt}, ${auditWho ai}, ${auditIp ai}, ${view sa :: Id Container}, ${view sa :: Segment}, ${view sa :: Id Asset})|]

slotAssetJSON :: SlotAsset -> JSON.Object
slotAssetJSON sa@SlotAsset{..} = assetJSON slotAsset JSON..++ catMaybes
  [ Just $ "container" JSON..= containerId (slotContainer assetSlot)
  , Range.isFull (slotSegment assetSlot) ?!> "segment" JSON..= slotSegment assetSlot
  , Just $ "permission" JSON..= (view sa :: Permission)
  , ("excerpt" JSON..=) <$> slotAssetExcerpt
  ]
