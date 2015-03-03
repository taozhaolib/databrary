{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.SQL
  ( selectQuery
  , tryUpdateOrInsert
  , updateOrInsert
  ) where

import Control.Monad (guard)
import Database.PostgreSQL.Typed.Protocol (PGError(..), pgMessageCode)
import Database.PostgreSQL.Typed.Query (PGQuery)

import Control.Applicative.Ops
import Databrary.DB
import Databrary.Model.SQL.Select

isDuplicateKeyException :: PGError -> Bool
isDuplicateKeyException (PGError e) = pgMessageCode e `elem` ["23505", "23P01"]

tryUpdateOrInsert :: (DBM m, PGQuery q a) => (PGError -> Maybe e) -> q -> q -> m (Either e (Int, [a]))
tryUpdateOrInsert err upd ins = dbTransaction uoi where
  err' e
    | isDuplicateKeyException e = Just Nothing
    | otherwise = Just <$> err e
  uoi = do
    u <- dbTryQuery err upd
    case u of
      Right (0, _) -> do
        dbExecuteSimple "SAVEPOINT pre_insert"
        i <- dbTryQuery err' ins
        case i of
          Left Nothing -> do
            dbExecuteSimple "ROLLBACK TO SAVEPOINT pre_insert"
            uoi
          Left (Just e) -> return $ Left e
          Right r -> return $ Right r
      _ -> return u

updateOrInsert :: (DBM m, PGQuery q a) => q -> q -> m (Int, [a])
-- updateOrInsert upd ins = either fail return <$> tryUpdateOrInsert (const Nothing) upd ins
updateOrInsert upd ins = dbTransaction uoi where
  uoi = do
    u@(n, _) <- dbRunQuery upd
    if n /= 0
      then return u
      else do
        dbExecuteSimple "SAVEPOINT pre_insert"
        i <- dbTryQuery (guard . isDuplicateKeyException) ins
        either (\() -> do
          dbExecuteSimple "ROLLBACK TO SAVEPOINT pre_insert"
          uoi)
          return i
