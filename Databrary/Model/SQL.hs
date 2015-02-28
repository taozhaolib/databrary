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

updateOrInsert :: (DBM m, PGQuery q a) => q -> q -> m (Int, [a])
updateOrInsert upd ins = dbTransaction uoi where
  -- uoi :: DBTransaction (Int, [a])
  uoi = do
    u@(n, _) <- dbRunQuery upd
    if n /= 0
      then return u
      else do
        dbExecuteSimple "SAVEPOINT pre_insert"
        i <- dbTryQuery ((() <?) . isDuplicateKeyException) ins
        either (\() -> do
          dbExecuteSimple "ROLLBACK TO SAVEPOINT pre_insert"
          uoi)
          return i
