{-# LANGUAGE OverloadedStrings #-}
module Databrary.Resource.DB
  ( DBConn
  , initDB
  , withDB
  , loadTPG
  ) where

import Control.Applicative ((<$>))
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Maybe (fromMaybe, isJust)
import Data.Pool (Pool, withResource, createPool)
import Database.PostgreSQL.Typed
import qualified Language.Haskell.TH as TH
import Network (PortID(..))

getPGDatabase :: C.Config -> IO PGDatabase
getPGDatabase conf = do
  host <- C.lookup conf "host"
  port <- C.lookupDefault (5432 :: Int) conf "port"
  sock <- C.lookupDefault "/tmp/.s.PGSQL.5432" conf "sock"
  user <- C.require conf "user"
  db <- C.lookupDefault user conf "db"
  passwd <- C.lookupDefault "" conf "pass"
  debug <- C.lookupDefault False conf "debug"
  return $ defaultPGDatabase
    { pgDBHost = fromMaybe "localhost" host
    , pgDBPort = if isJust host then PortNumber (fromIntegral port) else UnixSocket sock
    , pgDBName = db
    , pgDBUser = user
    , pgDBPass = passwd
    , pgDBDebug = debug
    }

newtype DBConn = PGPool (Pool PGConnection)

initDB :: C.Config -> IO DBConn
initDB conf = do
  db <- getPGDatabase conf
  PGPool <$> createPool
    (pgConnect db)
    pgDisconnect
    1 60 16

withDB :: DBConn -> (PGConnection -> IO a) -> IO a
withDB (PGPool p) = withResource p

loadTPG :: TH.DecsQ
loadTPG = useTPGDatabase =<< TH.runIO (getPGDatabase . C.subconfig "db" =<< C.load [C.Required "databrary.conf"])
