module Databrary.Resource.DB
  ( DBConn
  , initDB
  , HasDB(..)
  , liftDB
  , loadTPG
  ) where

import Control.Applicative ((<$>))
import Control.Lens (Lens', view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Pool (Pool, withResource, createPool)
import Database.PostgreSQL.Typed
import qualified Language.Haskell.TH as TH
import Network (PortID(..))

pgDatabase :: IO PGDatabase
pgDatabase = return defaultPGDatabase
  { pgDBPort = UnixSocket "/tmp/.s.PGSQL.5432"
  , pgDBUser = "dylan"
  , pgDBName = "databrary"
  }

newtype DBConn = PGPool (Pool PGConnection)

initDB :: IO DBConn
initDB = do
  db <- pgDatabase
  PGPool <$> createPool
    (pgConnect db)
    pgDisconnect
    1 60 16

class HasDB b where
  dbLens :: Lens' b DBConn

liftDB :: (MonadIO m, MonadReader b m, HasDB b) => (PGConnection -> IO a) -> m a
liftDB f = do
  PGPool p <- view dbLens
  liftIO $ withResource p f

loadTPG :: TH.DecsQ
loadTPG = useTPGDatabase =<< TH.runIO pgDatabase
