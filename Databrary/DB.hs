{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Databrary.DB
  ( MonadDB(..)
  , dbRunQuery
  , dbExecute
  , dbExecute1
  , dbQuery
  , dbQuery1
  , useTPG
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Database.PostgreSQL.Typed
import Database.PostgreSQL.Typed.TH (withTPGConnection)
import Database.PostgreSQL.Typed.Query
import qualified Language.Haskell.TH as TH
import System.IO.Unsafe (unsafePerformIO)

import Databrary.Resource.DB
import Databrary.Resource
import Databrary.Action

class MonadDB m where
  liftDB :: (PGConnection -> IO a) -> m a

instance (MonadIO m, HasResource m) => MonadDB m where
  liftDB f = do
    db <- getResource resourceDB
    liftIO $ withDB db f

dbRunQuery :: (MonadDB m, PGQuery q a) => q -> m (Int, [a])
dbRunQuery q = liftDB $ \c -> pgRunQuery c q

dbExecute :: (MonadDB m, PGQuery q ()) => q -> m Int
dbExecute q = liftDB $ \c -> pgExecute c q

dbExecute1 :: (Monad m, MonadDB m, PGQuery q ()) => q -> m ()
dbExecute1 q = do
  r <- dbExecute q
  when (r /= 1) $ fail $ "pgExecute1: " ++ show r ++ " rows"

dbQuery :: (MonadDB m, PGQuery q a) => q -> m [a]
dbQuery q = liftDB $ \c -> pgQuery c q

dbQuery1 :: (Monad m, MonadDB m, PGQuery q a) => q -> m (Maybe a)
dbQuery1 q = do
  r <- dbQuery q
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

instance MonadDB TH.Q where
  liftDB f = do
    _ <- useTPG
    TH.runIO $ withTPGConnection f
