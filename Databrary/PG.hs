module Databrary.PG
  ( pgDatabase
  ) where

import Database.PostgreSQL.Typed (PGDatabase(..), defaultPGDatabase)
import Network (PortID(..))

pgDatabase :: PGDatabase
pgDatabase = defaultPGDatabase
  { pgDBPort = UnixSocket "/tmp/.s.PGSQL.5432"
  , pgDBUser = "dylan"
  , pgDBName = "databrary"
  }

newtype PGConn = PGPool (Pool PGConnection)

class HasPG a where
  pgLens :: Lens' a PGConn

liftPG :: (MonadIO m, MonadReader b m, HasPG b) => (PG.PGConnection -> IO a) -> m a
liftPG f = do
  pg <- view pgLens
  liftIO $ withResource pg f
