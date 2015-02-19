{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Model.Metric
  ( module Databrary.Model.Metric.Types
  , getMetric
  , getMetric'
  , metricJSON
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (catMaybes)

import Control.Applicative.Ops
import Databrary.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Metric.Types
import Databrary.Model.Metric.Boot

useTPG

metrics :: [Metric]
metrics = $(loadMetrics)

metricsById :: IntMap.IntMap Metric
metricsById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ metricId a, a)) metrics

getMetric :: Id Metric -> Maybe Metric
getMetric (Id i) = IntMap.lookup (fromIntegral i) metricsById

getMetric' :: Id Metric -> Metric
getMetric' (Id i) = metricsById IntMap.! fromIntegral i

-- this is a hack, should be in database
metricLong :: Metric -> Bool
metricLong = ("description" ==) . metricName

metricJSON :: Metric -> JSON.Object
metricJSON m@Metric{..} = JSON.record metricId $ catMaybes
  [ Just $ "name" JSON..= metricName
  , Just $ "classification" JSON..= metricClassification
  , Just $ "type" JSON..= show metricType
  , "options" JSON..= metricOptions <!? null metricOptions
  , ("assumed" JSON..=) <$> metricAssumed
  , "long" JSON..= True <? metricLong m
  ]
