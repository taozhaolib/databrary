{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Record
  ( module Databrary.Model.Record.Types
  , lookupRecord
  , lookupVolumeRecords
  , addRecord
  , changeRecord
  , removeRecord
  , changeRecordMeasure
  , removeRecordMeasure
  , recordJSON
  ) where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)
import qualified Data.Text as T

import Control.Has (peek, view)
import Databrary.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party.Types
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.Model.Record.Types
import Databrary.Model.Record.SQL

useTPG

lookupRecord :: (MonadHasIdentity c m, DBM m) => Id Record -> m (Maybe Record)
lookupRecord ri = do
  ident <- peek
  dbQuery1 $(selectQuery (selectRecord 'ident) "$WHERE record.id = ${ri}")

lookupVolumeRecords :: DBM m => Volume -> m [Record]
lookupVolumeRecords vol =
  dbQuery $ fmap ($ vol) $(selectQuery selectVolumeRecord "$WHERE record.volume = ${volumeId vol}")

addRecord :: AuditM c m => Record -> m Record
addRecord br = do
  ident <- getAuditIdentity
  dbQuery1' $(insertRecord 'ident 'br)

changeRecord :: AuditM c m => Record -> m ()
changeRecord r = do
  ident <- getAuditIdentity
  dbExecute1 $(updateRecord 'ident 'r)

removeRecord :: AuditM c m => Record -> m ()
removeRecord r = do
  ident <- getAuditIdentity
  dbExecute1 $(deleteRecord 'ident 'r)

changeRecordMeasure :: AuditM c m => Measure -> m Measure
changeRecordMeasure m = do
  ident <- getAuditIdentity
  (_, [r]) <- updateOrInsert
    $(updateMeasure 'ident 'm)
    $(insertMeasure 'ident 'm)
  return r

removeRecordMeasure :: AuditM c m => Measure -> m Bool
removeRecordMeasure m = do
  ident <- getAuditIdentity
  (0 <) <$> dbExecute $(deleteMeasure 'ident 'm)

getRecordMeasures :: Record -> Measures
getRecordMeasures r = maybe [] filt $ readClassification (view r) (view r) where
  filt c = filter ((>= c) . view) $ recordMeasures r

measureJSONPair :: Measure -> JSON.Pair
measureJSONPair m = T.pack (show (metricId (measureMetric m))) JSON..= measureDatum m

recordJSON :: Record -> JSON.Object
recordJSON r@Record{..} = JSON.record recordId $ catMaybes
  [ Just $ "volume" JSON..= volumeId recordVolume
  , ("category" JSON..=) <$> fmap recordCategoryId recordCategory
  , Just $ "measures" JSON..= JSON.Object (JSON.object $ map measureJSONPair $ getRecordMeasures r)
  ]

