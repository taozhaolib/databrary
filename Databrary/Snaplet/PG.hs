{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
module Databrary.Snaplet.PG (
    PG
  , HasPG(..)
  , PGConfig(..)
  , pgDefaultConfig

  , pgInit
  , pgInit'

  , getPGConfig
  , getPGDatabase
  , loadPGDatabase
  , withPG
  , liftPG

  , PG.pgSQL
  , pgRunQuery
  , pgExecute
  , pgExecute1
  , pgQuery
  , pgQuery1

  , useTPG
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, local)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Pool (Pool, withResource, createPool)
import Data.Sequence (Seq)
import qualified Language.Haskell.TH as TH
import Network (PortID(..))
import Snap.Snaplet
import System.IO.Unsafe (unsafePerformIO)

import qualified Database.PostgreSQL.Typed as PG
import qualified Database.PostgreSQL.Typed.Query as PG
import Paths_databrary (getDataDir)


data PG
  = PGPool (Pool PG.PGConnection)
  | PGConn PG.PGConnection

class HasPG b where
  pgLens :: SnapletLens b PG

withPG :: HasPG b => Handler b PG a -> Handler b b a
withPG f = with pgLens $ do
  s <- ask
  case s of
    PGPool p -> withResource p (\c -> local (const $ PGConn c) f)
    PGConn _ -> f

liftPG :: HasPG b => (PG.PGConnection -> IO a) -> Handler b b a
liftPG f = with pgLens $ do
  s <- ask
  case s of
    PGPool p -> liftIO (withResource p f)
    PGConn c -> liftIO (f c)

data PGConfig = PGConfig
  { pgConfigDatabase :: PG.PGDatabase
  , pgConfigNumStripes :: Int
  , pgConfigIdleTime :: Double
  , pgConfigResources :: Int
  }

pgDefaultConfig :: PG.PGDatabase -> PGConfig
pgDefaultConfig db = PGConfig db 1 60 16

getPGDatabase :: C.Config -> IO PG.PGDatabase
getPGDatabase config = do
  host <- C.lookupDefault "localhost" config "host"
  port <- C.lookupDefault (5432 :: Int) config "port"
  sock <- C.lookup config "sock"
  user <- C.require config "user"
  db <- C.lookupDefault user config "db"
  passwd <- C.lookupDefault "" config "pass"
  debug <- C.lookupDefault False config "debug"
  return $ PG.PGDatabase
    { PG.pgDBHost = host
    , PG.pgDBPort = maybe (PortNumber (fromIntegral port)) UnixSocket sock
    , PG.pgDBName = db
    , PG.pgDBUser = user
    , PG.pgDBPass = passwd
    , PG.pgDBDebug = debug
    , PG.pgDBLogMessage = \_ -> return () -- something better?
    }

-- |Suitable for use with 'useTPGDatabase'
loadPGDatabase :: FilePath -> IO PG.PGDatabase
loadPGDatabase f = getPGDatabase =<< C.load [C.Required f]

getPGConfig :: C.Config -> IO PGConfig
getPGConfig config = do
  db <- getPGDatabase config
  let def = pgDefaultConfig db
  stripes <- C.lookupDefault (pgConfigNumStripes def) config "numStripes"
  idle <- C.lookupDefault (pgConfigIdleTime def) config "idleTime"
  resources <- C.lookupDefault (pgConfigResources def) config "maxResourcesPerStripe"
  return $ PGConfig db stripes idle resources

pgMake :: Initializer b PG PGConfig -> SnapletInit b PG
pgMake config = makeSnaplet "postgresql-typed" "PostgreSQL-Typed interface" (Just getDataDir) $ do
  c <- config
  liftIO $ PGPool <$> createPool (PG.pgConnect (pgConfigDatabase c)) PG.pgDisconnect
    (pgConfigNumStripes c) (realToFrac $ pgConfigIdleTime c) (pgConfigResources c)

pgInit :: SnapletInit b PG
pgInit = pgMake (liftIO . getPGConfig =<< getSnapletUserConfig)

pgInit' :: PGConfig -> SnapletInit b PG
pgInit' config = pgMake (return config)


pgRunQuery :: (HasPG b, PG.PGQuery q a) => q -> Handler b b (Int, Seq a)
pgRunQuery q = liftPG $ \c -> PG.pgRunQuery c q

pgExecute :: (HasPG b, PG.PGQuery q ()) => q -> Handler b b Int
pgExecute q = liftPG $ \c -> PG.pgExecute c q

pgExecute1 :: (HasPG b, PG.PGQuery q ()) => q -> Handler b b ()
pgExecute1 q = do
  r <- pgExecute q
  when (r /= 1) $ fail $ "pgExecute1: " ++ show r ++ " rows"

pgQuery :: (HasPG b, PG.PGQuery q a) => q -> Handler b b [a]
pgQuery q = liftPG $ \c -> PG.pgQuery c q

pgQuery1 :: (HasPG b, PG.PGQuery q a) => q -> Handler b b (Maybe a)
pgQuery1 q = do
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
    else PG.useTPGDatabase =<< TH.runIO (loadPGDatabase "snaplets/postgresql-typed/devel.cfg")
