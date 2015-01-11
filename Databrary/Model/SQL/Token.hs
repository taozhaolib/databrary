module Databrary.Model.SQL.Token
  ( tokenRow
  ) where

import Databrary.Model.SQL (Selector, selectColumns)
import Databrary.Model.Types.Token

tokenRow :: String -> Selector
tokenRow table = selectColumns 'Token table ["id", "expires"]
