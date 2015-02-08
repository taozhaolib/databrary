{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.RecordCategory.SQL
  ( recordCategoryRow
  , recordTemplateRow
  ) where

import Databrary.Model.RecordCategory.Types
import Databrary.Model.SQL

recordCategoryRow :: Selector -- RecordCategory
recordCategoryRow = selectColumns 'RecordCategory "record_category" ["id", "name"]

recordTemplateRow :: Selector -- (Id Metric, Bool)
recordTemplateRow = selectColumns '(,) "record_template" ["metric", "ident"]
