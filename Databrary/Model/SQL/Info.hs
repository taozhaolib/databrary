{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.SQL.Info
  ( lookupTableOID
  ) where

import Data.Monoid ((<>))
import Database.PostgreSQL.Typed.Types (OID)
import Database.PostgreSQL.Typed.Query (rawPGSimpleQuery)
import Database.PostgreSQL.Typed.Dynamic (pgLiteralRep, pgDecodeRep)

import Databrary.Service.DB

lookupTableOID :: MonadDB m => String -> m OID
lookupTableOID t = do
  [r] <- dbQuery1' $ rawPGSimpleQuery $ "SELECT oid FROM pg_class WHERE relname = " <> pgLiteralRep t
  return $ pgDecodeRep r
