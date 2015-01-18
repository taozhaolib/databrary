{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.SQL.Token
  ( sessionTokenSelector
  ) where

import Databrary.Model.SQL
import Databrary.Model.SQL.Party (accountSelector)
import Databrary.Model.SQL.Authorize (accessRow)
import Databrary.Model.Types.Token

tokenRow :: String -> Selector
tokenRow table = selectColumns 'Token table ["token", "expires"]

accountTokenRow :: String -> Selector
accountTokenRow table = selectJoin 'AccountToken 
  [ tokenRow table
  , joinOn (table ++ ".account = account.id") accountSelector
  ]

sessionTokenSelector :: Selector
sessionTokenSelector = selectJoin 'SessionToken 
  [ accountTokenRow "session"
  , joinOn "session.account = authorize_view.child AND authorize_view.parent = 0" (accessRow "authorize_view")
  ]
