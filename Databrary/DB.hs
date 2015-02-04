{-# LANGUAGE DefaultSignatures, GeneralizedNewtypeDeriving #-}
module Databrary.DB
  ( DBM
  , dbRunQuery
  , dbExecute
  , dbExecute1
  , dbQuery
  , dbQuery1
  , dbTransaction
  , DBTransaction
  , useTPG
  ) where

import Control.Applicative (Applicative)
import Control.Exception (onException)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT(..))
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Database.PostgreSQL.Typed.TH (withTPGConnection)
import Database.PostgreSQL.Typed.Query
import Database.PostgreSQL.Typed.Protocol (PGConnection, pgSimpleQuery)
import qualified Language.Haskell.TH as TH
import System.IO.Unsafe (unsafePerformIO)

import Control.Has (Has)
import Databrary.Resource.DB
import Databrary.Resource

class (Functor m, Applicative m, Monad m) => DBM m where
  liftDB :: (PGConnection -> IO a) -> m a
  default liftDB :: (MonadIO m, ResourceM c m) => (PGConnection -> IO a) -> m a
  liftDB f = do
    db <- getResource resourceDB
    liftIO $ withDB db f

instance (Functor m, Applicative m, MonadIO m, Has Resource c) => DBM (ReaderT c m)

dbRunQuery :: (DBM m, PGQuery q a) => q -> m (Int, [a])
dbRunQuery q = liftDB $ \c -> pgRunQuery c q

dbExecute :: (DBM m, PGQuery q ()) => q -> m Int
dbExecute q = liftDB $ \c -> pgExecute c q

dbExecute1 :: (DBM m, PGQuery q ()) => q -> m ()
dbExecute1 q = do
  r <- dbExecute q
  when (r /= 1) $ fail $ "pgExecute1: " ++ show r ++ " rows"

dbQuery :: (DBM m, PGQuery q a) => q -> m [a]
dbQuery q = liftDB $ \c -> pgQuery c q

dbQuery1 :: (DBM m, PGQuery q a) => q -> m (Maybe a)
dbQuery1 q = do
  r <- dbQuery q
  case r of
    [] -> return $ Nothing
    [x] -> return $ Just x
    _ -> fail "pgQuery1: too many results"

newtype DBTransaction a = DBTransaction { runDBTransaction :: ReaderT PGConnection IO a } deriving (Functor, Applicative, Monad, MonadIO)

instance DBM DBTransaction where
  liftDB = DBTransaction . ReaderT

dbTransaction :: DBM m => DBTransaction a -> m a
dbTransaction f = liftDB $ \c -> do
  pgSimpleQuery c "BEGIN"
  onException (do
    r <- runReaderT (runDBTransaction f) c
    pgSimpleQuery c "COMMIT"
    return r)
    (pgSimpleQuery c "ROLLBACK")

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
