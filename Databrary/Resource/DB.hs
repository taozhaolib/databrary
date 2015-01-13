module Databrary.Resource.DB
  ( DBConn
  , initDB
  , withDB
  , loadTPG
  ) where

import Control.Applicative ((<$>))
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

withDB :: DBConn -> (PGConnection -> IO a) -> IO a
withDB (PGPool p) = withResource p

loadTPG :: TH.DecsQ
loadTPG = useTPGDatabase =<< TH.runIO pgDatabase
