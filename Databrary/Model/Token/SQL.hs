{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Databrary.Model.Token.SQL
  ( selectSessionToken
  , selectUploadToken
  ) where

import qualified Data.Text as T

import Databrary.Model.SQL
import Databrary.Model.Party.Types
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

selectSessionToken :: Selector -- @'SessionToken'@
selectSessionToken =
  addSelects 'SessionToken (accountTokenRow "session") ["session.superuser"]

makeUploadToken :: Token -> T.Text -> SiteAuth -> UploadToken
makeUploadToken t n u = UploadToken (AccountToken t u) n

selectUploadToken :: Selector -- @'SiteAuth' -> 'SessionToken'@
selectUploadToken =
  addSelects 'makeUploadToken (tokenRow "upload") ["upload.filename"]
