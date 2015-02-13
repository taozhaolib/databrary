{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Databrary.Model.Token.SQL
  ( selectSessionToken
  ) where

import Databrary.Model.SQL
import Databrary.Model.Party.SQL (selectSiteAuth)
import Databrary.Model.Token.Types

tokenRow :: String -- ^ table name
  -> Selector -- ^ @'Token'@
tokenRow table = selectColumns 'Token table ["token", "expires"]

accountTokenRow :: String -- ^ table name
  -> Selector -- ^ @'AccountToken'@
accountTokenRow table = selectJoin 'AccountToken 
  [ tokenRow table
  , joinOn (table ++ ".account = account.id") selectSiteAuth
  ]

selectSessionToken :: Selector
selectSessionToken =
  addSelects 'SessionToken (accountTokenRow "session") ["session.superuser"]
