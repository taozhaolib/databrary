{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Record.SQL
  ( selectVolumeRecord
  , selectRecord
  , insertRecord
  , updateRecord
  , deleteRecord
  , insertMeasure
  , updateMeasure
  , deleteMeasure
  ) where

import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Consent.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.Model.Record.Types

parseMeasure :: Record -> BSC.ByteString -> Measure
parseMeasure r s = (\(m, d) -> Measure r (getMetric' (Id (read (BSC.unpack m)))) (BSC.tail d))
  $ BSC.splitAt (fromMaybe (error $ "parseMeasure: " ++ BSC.unpack s) $ BSC.elemIndex ':' s) s

makeRecord :: Id Record -> Maybe (Id RecordCategory) -> [Maybe BSC.ByteString] -> Maybe Consent -> Volume -> Record
makeRecord i c ms p v = r where
  r = Record i v (fmap getRecordCategory' c) p (map (parseMeasure r . fromMaybe (error "NULL record.measure")) ms)

recordRow :: Selector -- ^ @'Consent' -> 'Volume' -> 'Record'@
recordRow = fromMap ("record_measures AS " ++) $
  selectColumns 'makeRecord "record" ["id", "category", "measures"]

selectVolumeRecord :: Selector -- ^ @'Volume' -> 'Record'@
selectVolumeRecord = addSelects '($) recordRow ["record_consent(record.id)"]

selectRecord :: TH.Name -- ^ @'Identity'@
  -> Selector -- ^ @'Record'@
selectRecord ident = selectJoin '($)
  [ selectVolumeRecord
  , joinOn "record.volume = volume.id" $ selectVolume ident
  ]

recordKeys :: String -- ^ @'Record'@
  -> [(String, String)]
recordKeys r =
  [ ("id", "${recordId " ++ r ++ "}") ]

recordSets :: String -- ^ @'Record'@
  -> [(String, String)]
recordSets r =
  [ ("volume", "${volumeId (recordVolume " ++ r ++ ")}")
  , ("category", "${fmap recordCategoryId (recordCategory " ++ r ++ ")}")
  ]

setRecordId :: Record -> Id Record -> Record
setRecordId r i = r{ recordId = i }

insertRecord :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Record'@
  -> TH.ExpQ -- ^ @'Record'@
insertRecord ident r = auditInsert ident "record"
  (recordKeys (nameRef r) ++ recordSets (nameRef r))
  (Just $ selectOutput $ selectMap ((TH.VarE 'setRecordId `TH.AppE` TH.VarE r) `TH.AppE`) $ selector "record" "id")

updateRecord :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Record'@
  -> TH.ExpQ -- ^ @()@
updateRecord ident r = auditUpdate ident "record"
  (recordSets (nameRef r))
  (whereEq $ recordKeys (nameRef r))
  Nothing

deleteRecord :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Record'@
  -> TH.ExpQ -- ^ @()@
deleteRecord ident r = auditDelete ident "record"
  (whereEq $ recordKeys (nameRef r))
  Nothing

setMeasureDatum :: Measure -> MeasureDatum -> Measure
setMeasureDatum m d = m{ measureDatum = d }

measureDatumRow :: Selector -- ^ @'MeasureDatum'@
measureDatumRow = selector "measure" "measure.datum"

measureKeys :: String -- ^ @'Measure'@
  -> [(String, String)]
measureKeys m =
  [ ("record", "${recordId (measureRecord " ++ m ++ ")}")
  , ("metric", "${metricId (measureMetric " ++ m ++ ")}")
  ]

measureSets :: String -- ^ @'Record'@
  -> [(String, String)]
measureSets r =
  [ ("datum", "${measureDatum " ++ r ++ "}")
  ]

insertMeasure :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Measure'@
  -> TH.ExpQ -- ^ @'Measure'@
insertMeasure ident m = auditInsert ident "!measure"
  (measureKeys (nameRef m) ++ measureSets (nameRef m))
  (Just $ selectOutput $ selectMap ((TH.VarE 'setMeasureDatum `TH.AppE` TH.VarE m) `TH.AppE`) measureDatumRow)

updateMeasure :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Measure'@
  -> TH.ExpQ -- ^ @'Measure'@
updateMeasure ident m = auditUpdate ident "!measure"
  (measureSets (nameRef m))
  (whereEq $ measureKeys (nameRef m))
  (Just $ selectOutput $ selectMap ((TH.VarE 'setMeasureDatum `TH.AppE` TH.VarE m) `TH.AppE`) measureDatumRow)

deleteMeasure :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Measure'@
  -> TH.ExpQ -- ^ @()@
deleteMeasure ident m = auditDelete ident "measure"
  (whereEq $ measureKeys (nameRef m))
  Nothing

