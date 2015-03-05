{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Databrary.Model.Token.SQL
  ( selectLoginToken
  , selectSession
  , selectUpload
  ) where

import qualified Data.ByteString as BS

import Databrary.Model.SQL.Select
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

selectLoginToken :: Selector -- @'Session'@
selectLoginToken =
  addSelects 'LoginToken (accountTokenRow "login_token") ["login_token.password"]

selectSession :: Selector -- @'Session'@
selectSession =
  addSelects 'Session (accountTokenRow "session") ["session.superuser"]

makeUpload :: Token -> BS.ByteString -> SiteAuth -> Upload
makeUpload t n u = Upload (AccountToken t u) n

selectUpload :: Selector -- @'SiteAuth' -> 'Upload'@
selectUpload =
  addSelects 'makeUpload (tokenRow "upload") ["upload.filename"]
