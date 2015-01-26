{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances, ConstraintKinds #-}
module Databrary.DB
  ( DBM
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

class (Functor m, Monad m) => DBM m where
  liftDB :: (PGConnection -> IO a) -> m a

instance (MonadIO m, ResourceM c m) => DBM m where
  liftDB f = do
    db <- getResource resourceDB
    liftIO $ withDB db f

dbRunQuery :: (DBM m, PGQuery q a) => q -> m (Int, [a])
dbRunQuery q = liftDB $ \c -> pgRunQuery c q

dbExecute :: (DBM m, PGQuery q ()) => q -> m Int
dbExecute q = liftDB $ \c -> pgExecute c q

dbExecute1 :: (Monad m, DBM m, PGQuery q ()) => q -> m ()
dbExecute1 q = do
  r <- dbExecute q
  when (r /= 1) $ fail $ "pgExecute1: " ++ show r ++ " rows"

dbQuery :: (DBM m, PGQuery q a) => q -> m [a]
dbQuery q = liftDB $ \c -> pgQuery c q

dbQuery1 :: (Monad m, DBM m, PGQuery q a) => q -> m (Maybe a)
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

instance DBM TH.Q where
  liftDB f = do
    _ <- useTPG
    TH.runIO $ withTPGConnection f
