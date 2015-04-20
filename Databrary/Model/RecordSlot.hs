{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds #-}
module Databrary.Model.RecordSlot
  ( module Databrary.Model.RecordSlot.Types
  , lookupRecordSlots
  , lookupSlotRecords
  , lookupContainerRecords
  , moveRecordSlot
  , recordSlotJSON
  ) where

import Control.Monad (guard, liftM2)
import Data.Maybe (catMaybes)
import qualified Database.PostgreSQL.Typed.Range as Range
import Database.PostgreSQL.Typed.Types (PGTypeName(..))

import Databrary.Ops
import Databrary.Has (view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Segment
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Metric
import Databrary.Model.Record
import Databrary.Model.Age
import Databrary.Model.Measure
import Databrary.Model.SQL
import Databrary.Model.RecordSlot.Types
import Databrary.Model.RecordSlot.SQL

lookupRecordSlots :: (MonadDB m) => Record -> m [RecordSlot]
lookupRecordSlots r =
  dbQuery $ ($ r) <$> $(selectQuery selectRecordSlotRecord "$WHERE slot_record.record = ${recordId r}")

lookupSlotRecords :: (MonadDB m) => Slot -> m [RecordSlot]
lookupSlotRecords (Slot c s) =
  dbQuery $ ($ c) <$> $(selectQuery selectContainerSlotRecord "$WHERE slot_record.container = ${containerId c} AND slot_record.segment && ${s}")

lookupContainerRecords :: (MonadDB m) => Container -> m [RecordSlot]
lookupContainerRecords = lookupSlotRecords . containerSlot

moveRecordSlot :: (MonadAudit c m) => RecordSlot -> Segment -> m Bool
moveRecordSlot rs@RecordSlot{ recordSlot = s@Slot{ slotSegment = src } } dst = do
  ident <- getAuditIdentity
  either (const False) ((0 <) . fst)
    <$> case (Range.isEmpty (segmentRange src), Range.isEmpty (segmentRange dst)) of
    (True,  True) -> return $ Right (0, [])
    (False, True) -> Right <$> dbRunQuery $(deleteSlotRecord 'ident 'rs)
    (True,  False) -> dbTryQuery err $(insertSlotRecord 'ident 'rd)
    (False, False) -> dbTryQuery err $(updateSlotRecord 'ident 'rs 'dst)
  where
  rd = rs{ recordSlot = s{ slotSegment = dst } }
  err = guard . isExclusionViolation

recordSlotAge :: RecordSlot -> Maybe Age
recordSlotAge rs@RecordSlot{..} =
  clip <$> liftM2 age (decodeMeasure (PGTypeProxy :: PGTypeName "date") =<< getMeasure birthdateMetric (recordMeasures slotRecord)) (containerDate $ slotContainer recordSlot)
  where
  clip a
    | dataPermission (view rs) (view birthdateMetric) (view rs) < PermissionREAD = a `min` ageLimit
    | otherwise = a
  ageLimit = yearsAge (90 :: Int)

recordSlotJSON :: RecordSlot -> JSON.Object
recordSlotJSON rs@RecordSlot{..} = JSON.record (recordId slotRecord) $ catMaybes
  [ segmentFull (slotSegment recordSlot) ?!> ("segment" JSON..= slotSegment recordSlot)
  , ("age" JSON..=) <$> recordSlotAge rs
  ]
