module Databrary.Model.SQL.Token
  ( tokenRow
  ) where

import Databrary.Model.SQL (Selector, selectColumns)
import Databrary.Model.SQL.Party (accountSelector)
import Databrary.Model.Types.Token

tokenRow :: String -> Selector
tokenRow table = selectColumns 'Token table ["id", "expires"]

accountTokenRow :: String -> Selector
accountTokenRow table = selectJoin 'AccountToken 
  [ tokenRow table
  , joinOn (table + ".account = account.id") accountSelector
  ]

sessionTokenSelector :: Selector
sessionTokenSelector = selectJoin 'SessionToken 
  [ accountTokenRow "session"
  , joinOn "session.account = authorize_view.child AND authorize_view.parent = 0"
  ]
