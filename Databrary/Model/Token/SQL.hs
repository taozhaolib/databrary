{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Databrary.Model.Token.SQL
  ( selectSessionToken
  ) where

import Databrary.Model.SQL
import Databrary.Model.Party.SQL (selectAccount)
import Databrary.Model.Authorize.SQL (accessRow)
import Databrary.Model.Token.Types

tokenRow :: String -> Selector
tokenRow table = selectColumns 'Token table ["token", "expires"]

accountTokenRow :: String -> Selector
accountTokenRow table = selectJoin 'AccountToken 
  [ tokenRow table
  , joinOn (table ++ ".account = account.id") selectAccount
  ]

selectSessionToken :: Selector
selectSessionToken = selectJoin '($)
  [ addSelects 'SessionToken (accountTokenRow "session") ["session.superuser"]
  , joinOn "session.account = authorize_view.child AND authorize_view.parent = 0" (accessRow "authorize_view")
  ]
