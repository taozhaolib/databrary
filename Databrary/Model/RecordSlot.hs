{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, DataKinds, ViewPatterns #-}
module Databrary.Model.RecordSlot
  ( module Databrary.Model.RecordSlot.Types
  , lookupRecordSlots
  , lookupSlotRecords
  , lookupContainerRecords
  , lookupVolumeContainersRecordIds
  , moveRecordSlot
  , recordSlotJSON
  ) where

import Control.Monad (guard, liftM2)
import Data.Maybe (catMaybes)
import qualified Database.PostgreSQL.Typed.Range as Range
import Database.PostgreSQL.Typed.Types (PGTypeName(..))

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.Id.Types
import Databrary.Model.Segment
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Volume.Types
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

groupSlotRecordIds :: [(Slot, Maybe (Id Record))] -> [(Container, [(Segment, Id Record)])]
groupSlotRecordIds [] = []
groupSlotRecordIds ((s, Nothing) : l) = (slotContainer s, []) : groupSlotRecordIds l
groupSlotRecordIds (sr@(Slot{ slotContainer = c }, _) : (span ((containerId c ==) . containerId . slotContainer . fst) -> (cl, l))) =
  (c, [ (s, r) | (Slot{ slotSegment = s }, Just r) <- sr : cl ]) : groupSlotRecordIds l

lookupVolumeContainersRecordIds :: (MonadDB m) => Volume -> m [(Container, [(Segment, Id Record)])]
lookupVolumeContainersRecordIds v =
  groupSlotRecordIds <$>
    dbQuery (($ v) <$> $(selectQuery selectVolumeSlotRecordId "$WHERE container.volume = ${volumeId v} ORDER BY container.id"))

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
    | dataPermission rs == PermissionNONE = a `min` ageLimit
    | otherwise = a
  ageLimit = yearsAge (90 :: Int)

recordSlotJSON :: RecordSlot -> JSON.Object
recordSlotJSON rs@RecordSlot{..} = JSON.record (recordId slotRecord) $ catMaybes
  [ segmentJSON (slotSegment recordSlot)
  , ("age" JSON..=) <$> recordSlotAge rs
  ]
