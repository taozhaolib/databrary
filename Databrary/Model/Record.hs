{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Record
  ( module Databrary.Model.Record.Types
  , blankRecord
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
import Control.Monad (guard)
import Data.Function (on)
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Protocol (PGError(..), pgMessageCode)

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

blankRecord :: Volume -> Record
blankRecord vol = Record
  { recordId = error "blankRecord"
  , recordVolume = vol
  , recordCategory = Nothing
  , recordConsent = Nothing
  , recordMeasures = []
  }

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

measureOrder :: Measure -> Measure -> Ordering
measureOrder = compare `on` (metricId . measureMetric)

rmMeasure :: Measure -> Record
rmMeasure m@Measure{ measureRecord = rec } = rec{ recordMeasures = upd $ recordMeasures rec } where
  upd [] = [m]
  upd l@(m':l') = case m `measureOrder` m' of
    GT -> m':upd l'
    EQ -> l'
    LT -> l

upMeasure :: Measure -> Record
upMeasure m@Measure{ measureRecord = rec } = rec{ recordMeasures = upd $ recordMeasures rec } where
  upd [] = [m]
  upd l@(m':l') = case m `measureOrder` m' of
    GT -> m':upd l'
    EQ -> m:l'
    LT -> m:l

isInvalidInputException :: PGError -> Bool
isInvalidInputException (PGError e) = pgMessageCode e `elem` ["22007", "22008", "22P02"]

changeRecordMeasure :: AuditM c m => Measure -> m (Maybe Record)
changeRecordMeasure m = do
  ident <- getAuditIdentity
  r <- tryUpdateOrInsert (guard . isInvalidInputException)
    $(updateMeasure 'ident 'm)
    $(insertMeasure 'ident 'm)
  case r of
    Left () -> return Nothing
    Right (_, [d]) -> return $ Just $ upMeasure d
    Right (n, _) -> fail $ "changeRecordMeasure: " ++ show n ++ " rows"

removeRecordMeasure :: AuditM c m => Measure -> m Record
removeRecordMeasure m = do
  ident <- getAuditIdentity
  r <- dbExecute $(deleteMeasure 'ident 'm)
  return $ if r > 0
    then rmMeasure m
    else measureRecord m

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

