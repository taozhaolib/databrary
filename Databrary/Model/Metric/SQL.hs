{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Metric.SQL
  ( metricRow
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Databrary.Model.Id.Types
import Databrary.Model.Release.Types
import Databrary.Model.Metric.Types
import Databrary.Model.SQL.Select

makeMetric :: Id Metric -> T.Text -> Maybe Release -> MeasureType -> Maybe [Maybe MeasureDatum] -> Maybe MeasureDatum -> Metric
makeMetric i n c t o a = Metric i n c t (maybe [] (map (fromMaybe (error "NULL measure.option"))) o) a

metricRow :: Selector -- Metric
metricRow = selectColumns 'makeMetric "metric" ["id", "name", "release", "type", "options", "assumed"]
