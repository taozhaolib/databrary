{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Metric.Boot
  ( loadMetrics
  ) where

import Database.PostgreSQL.Typed.Array ()
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Databrary.Service.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Metric.SQL

useTPG

loadMetrics :: TH.ExpQ
loadMetrics =
  TH.lift =<< dbQuery $(selectQuery metricRow "ORDER BY id")
