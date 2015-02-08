{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Measure.Types
  ( Measure(..)
  , MonadHasMeasure
  ) where

import Control.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Record.Types
import Databrary.Model.Metric.Types

data Measure = Measure
  { measureRecord :: Record 
  , measureMetric :: Metric
  , measureDatum :: MeasureDatum
  }

instance Kinded Measure where
  kindOf _ = "measure"

makeHasRec ''Measure ['measureRecord, 'measureMetric]
