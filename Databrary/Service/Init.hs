{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Service.Init
  ( Service
  , withService
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import qualified Data.Configurator as C
import Data.Time.Clock (getCurrentTime)
import System.FilePath.Posix ((</>))

import Paths_databrary (getSysconfDir)
import Databrary.Service.DB (initDB, finiDB)
import Databrary.Service.Entropy (initEntropy, finiEntropy)
import Databrary.HTTP.Client (initHTTPClient, finiHTTPClient)
import Databrary.Store.Service (initStorage)
import Databrary.Service.Passwd (initPasswd)
import Databrary.Service.Log (initLogs, finiLogs)
import Databrary.Web.Service (initWeb)
import Databrary.Media.AV (initAV)
import Databrary.Service.Types

initService :: IO Service
initService = do
  etc <- getSysconfDir
  conf <- C.load [C.Required (etc </> "databrary.conf")]
  Service conf
    <$> getCurrentTime
    <*> initLogs (C.subconfig "log" conf)
    <*> C.require conf "secret"
    <*> initEntropy
    <*> initPasswd
    <*> initDB (C.subconfig "db" conf)
    <*> initStorage (C.subconfig "store" conf)
    <*> initWeb
    <*> initAV
    <*> initHTTPClient

finiService :: Service -> IO ()
finiService Service{..} = do
  finiHTTPClient serviceHTTPClient
  finiDB serviceDB
  finiEntropy serviceEntropy
  finiLogs serviceLogs

withService :: (Service -> IO a) -> IO a
withService = bracket initService finiService
