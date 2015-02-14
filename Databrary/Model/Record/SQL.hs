{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Record.SQL
  ( selectVolumeRecord
  , selectRecord
  ) where

import Control.Arrow ((***))
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Permission.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Volume.SQL
import Databrary.Model.RecordCategory
import Databrary.Model.Metric
import Databrary.Model.Record.Types

makeMeasure :: Record -> Id Metric -> MeasureDatum -> Measure
makeMeasure r m = Measure r (getMetric' m)

parseMeasure :: Record -> BSC.ByteString -> Measure
parseMeasure r s = uncurry (makeMeasure r)
  $ Id . read . BSC.unpack *** BSC.tail
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
