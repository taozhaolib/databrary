{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Service.Init
  ( loadConfig
  , withService
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Time.Clock (getCurrentTime)
import System.FilePath.Posix ((</>))

import Paths_databrary (getSysconfDir, getDataFileName)
import Databrary.Service.DB (initDB, finiDB)
import Databrary.Service.Entropy (initEntropy, finiEntropy)
import Databrary.HTTP.Client (initHTTPClient, finiHTTPClient)
import Databrary.Store.Service (initStorage)
import Databrary.Service.Passwd (initPasswd)
import Databrary.Service.Log (initLogs, finiLogs)
import Databrary.Service.Messages (initMessages)
import Databrary.Web.Service (initWeb)
import Databrary.Media.AV (initAV)
import Databrary.Service.Types

loadConfig :: IO C.Config
loadConfig = do
  etc <- getSysconfDir
  msg <- getDataFileName "messages.conf"
  C.loadGroups [("message", C.Optional msg), ("", C.Required (etc </> "databrary.conf")), ("", C.Optional (etc </> "local.conf"))]

initService :: C.Config -> IO Service
initService conf = Service
  <$> getCurrentTime
  <*> initLogs (C.subconfig "log" conf)
  <*> C.require conf "secret"
  <*> initMessages (C.subconfig "message" conf)
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

withService :: C.Config -> (Service -> IO a) -> IO a
withService c = bracket (initService c) finiService
