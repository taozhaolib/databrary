{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.Record.Types
  ( Record(..)
  , MonadHasRecord
  , Measure(..)
  , MonadHasMeasure
  , Measures
  ) where

import Control.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Consent.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Metric.Types
import Databrary.Model.RecordCategory.Types

type instance IdType Record = Int32

data Record = Record
  { recordId :: Id Record
  , recordVolume :: Volume
  , recordCategory :: Maybe RecordCategory
  , recordConsent :: Maybe Consent
  , recordMeasures :: Measures
  }

instance Kinded Record where
  kindOf _ = "record"

data Measure = Measure
  { measureRecord :: Record 
  , measureMetric :: Metric
  , measureDatum :: !MeasureDatum
  }

instance Kinded Measure where
  kindOf _ = "measure"

type Measures = [Measure]

makeHasRec ''Record ['recordId, 'recordVolume, 'recordCategory, 'recordConsent]
makeHasRec ''Measure ['measureRecord, 'measureMetric]
