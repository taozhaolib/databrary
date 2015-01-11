module Databrary.Model.SQL.Authorize
  (
  ) where

import Databrary.Model.Types.Authorize

accessRow :: String -> Selector
accessRow table = selectColumns 'Access ["site", "member"]
