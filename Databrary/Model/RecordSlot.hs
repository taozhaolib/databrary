{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.RecordSlot
  ( module Databrary.Model.RecordSlot.Types
  , lookupRecordSlots
  , lookupContainerRecords
  -- , changeRecordSlot
  , recordSlotJSON
  ) where

import Control.Monad (when, liftM2)
import Data.Maybe (catMaybes, isNothing)
import qualified Database.PostgreSQL.Typed.Range as Range

import Control.Applicative.Ops
import Control.Has (view)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.Time
import Databrary.Model.Permission
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Metric
import Databrary.Model.Record
import Databrary.Model.SQL
import Databrary.Model.RecordSlot.Types
import Databrary.Model.RecordSlot.SQL

lookupRecordSlots :: (DBM m) => Record -> m [RecordSlot]
lookupRecordSlots r =
  dbQuery $ ($ r) <$> $(selectQuery selectRecordSlotRecord "$WHERE slot_record.record = ${recordId r}")

lookupContainerRecords :: (DBM m) => Container -> m [RecordSlot]
lookupContainerRecords c =
  dbQuery $ ($ c) <$> $(selectQuery selectContainerSlotRecord "$WHERE slot_record.container = ${containerId c}")

{-
changeRecordSlot :: (MonadAudit c m) => RecordSlot -> m Bool
changeRecordSlot as = do
  ident <- getAuditIdentity
  (0 <) <$> if isNothing (recordSlot as)
    then dbExecute $(deleteSlotRecord 'ident 'as)
    else do
      (r, _) <- updateOrInsert
        $(updateSlotRecord 'ident 'as)
        $(insertSlotRecord 'ident 'as)
      when (r /= 1) $ fail $ "changeRecordSlot: " ++ show r ++ " rows"
      return r

recordSlotAge :: RecordSlot -> Maybe Age
recordSlotAge rs@RecordSlot{..} =
  clip <$> liftM2 age (measureDatum <$> getMeasure birthdateMetric (recordMeasures slotRecord)) (containerDate $ slotContainer recordSlot)
  where
  clip a
    | dataPermission (view rs) (view birthdateMetric) (view rs) < PermissionREAD = a `min` ageLimit
    | otherwise = a
  ageLimit = yearsAge 90
      -}

recordSlotJSON :: RecordSlot -> JSON.Object
recordSlotJSON rs@RecordSlot{..} = JSON.record (recordId slotRecord) $ catMaybes
  [ Range.isFull (slotSegment recordSlot) ?!> ("segment" JSON..= slotSegment recordSlot)
  -- , "age"
  ]
