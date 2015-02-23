{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.SQL
  ( selectQuery
  , updateOrInsert
  ) where

import Database.PostgreSQL.Typed.Protocol (PGError(..), pgMessageCode)
import Database.PostgreSQL.Typed.Query (PGQuery)

import Control.Applicative.Ops
import Databrary.DB
import Databrary.Model.SQL.Select

isDuplicateKeyException :: PGError -> Bool
isDuplicateKeyException (PGError e) = pgMessageCode e `elem` ["23505", "23P01"]

updateOrInsert :: (DBM m, PGQuery q ()) => q -> q -> m Int
updateOrInsert upd ins = dbTransaction uoi where
  uoi :: DBTransaction Int
  uoi = do
    u <- dbExecute upd
    if u /= 0
      then return u
      else do
        dbExecuteSimple "SAVEPOINT pre_insert"
        i <- dbTryExecute ((() <?) . isDuplicateKeyException) ins
        either (\() -> do
          dbExecuteSimple "ROLLBACK TO SAVEPOINT pre_insert"
          uoi)
          return i
