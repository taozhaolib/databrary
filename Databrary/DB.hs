module Databrary.DB
  ( dbRunQuery
  , dbExcute
  , dbExecute1
  , dbQuery
  , dbQuery1
  , useTPG
  ) where

import Database.PostgreSQL.Typed
import Network (PortID(..))

import Databrary.Resource.DB

dbRunQuery :: (MonadIO m, MonadReader b m, HasDB b, PG.PGQuery q a) => q -> m (Int, Seq a)
dbRunQuery q = liftDB $ \c -> pgRunQuery c q

dbExecute :: (MonadIO m, MonadReader b m, HasPG b, PG.PGQuery q ()) => q -> m Int
dbExecute q = liftDB $ \c -> PG.pgExecute c q

dbExecute1 :: (MonadIO m, MonadReader b m, HasPG b, PG.PGQuery q ()) => q -> m ()
dbExecute1 q = do
  r <- pgExecute q
  when (r /= 1) $ fail $ "pgExecute1: " ++ show r ++ " rows"

dbQuery :: (MonadIO m, MonadReader b m, HasPG b, PG.PGQuery q a) => q -> m [a]
dbQuery q = liftDB $ \c -> PG.pgQuery c q

dbQuery1 :: (MonadIO m, MonadReader b m, HasPG b, PG.PGQuery q a) => q -> m (Maybe a)
dbQuery1 q = do
  r <- pgQuery q
  case r of
    [] -> return $ Nothing
    [x] -> return $ Just x
    _ -> fail "pgQuery1: too many results"

{-# NOINLINE usedTPG #-}
usedTPG :: IORef Bool
usedTPG = unsafePerformIO $ newIORef False
useTPG :: TH.DecsQ
useTPG = do
  d <- TH.runIO $ atomicModifyIORef' usedTPG ((,) True)
  if d
    then return []
    else loadTPG
