{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.SlotAsset
  ( module Databrary.Model.SlotAsset.Types
  , lookupSlotAsset
  , auditSlotAssetDownload
  ) where

import Database.PostgreSQL.Typed (pgSQL)

import Control.Has (peek, see)
import Databrary.DB
import Databrary.Model.Time
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Container.Types
import Databrary.Model.Asset.Types
import Databrary.Model.Audit
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.SlotAsset.Types
import Databrary.Model.SlotAsset.SQL

lookupSlotAsset :: (MonadHasIdentity c m, DBM m) => Id Container -> Id Asset -> m (Maybe SlotAsset)
lookupSlotAsset ci ai = do
  ident <- peek
  dbQuery1 $(selectQuery (selectSlotAsset 'ident) "$WHERE container.id = ${ci} AND asset.id = ${ai}")

auditSlotAssetDownload :: AuditM c m => Bool -> SlotAsset -> m ()
auditSlotAssetDownload success sa = do
  ai <- getAuditIdentity
  dbExecute1 [pgSQL|INSERT INTO audit.slot_asset (audit_action, audit_user, audit_ip, container, segment, asset) VALUES
    (${if success then AuditActionOpen else AuditActionAttempt}, ${auditWho ai}, ${auditIp ai}, ${see sa :: Id Container}, ${see sa :: Segment}, ${see sa :: Id Asset})|]
