{-# LANGUAGE OverloadedStrings, TemplateHaskell, RecordWildCards #-}
module Databrary.Service
  ( Service(..)
  , MonadHasService
  , withService
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Time.Clock (getCurrentTime)

import Databrary.Has (makeHasRec)
import Databrary.Service.DB (DBConn, initDB, finiDB)
import Databrary.Service.Entropy (Entropy, initEntropy, finiEntropy)
import Databrary.HTTP.Client (HTTPClient, initHTTPClient, finiHTTPClient)
import Databrary.Store.Service (Storage, initStorage)
import Databrary.Service.Passwd (Passwd, initPasswd)
import Databrary.Service.Log (Logs, initLogs, finiLogs)
import Databrary.Media.AV (AV, initAV)
import Databrary.Model.Time

data Service = Service
  { serviceConfig :: !C.Config
  , serviceStartTime :: !Timestamp
  , serviceLogs :: !Logs
  , serviceSecret :: !BS.ByteString
  , serviceEntropy :: !Entropy
  , servicePasswd :: !Passwd
  , serviceDB :: !DBConn
  , serviceStorage :: !Storage
  , serviceAV :: !AV
  , serviceHTTPClient :: !HTTPClient
  }

makeHasRec ''Service ['serviceConfig, 'serviceDB, 'serviceEntropy, 'serviceHTTPClient, 'serviceStorage, 'servicePasswd, 'serviceAV]

initService :: IO Service
initService = do
  conf <- C.load [C.Required "databrary.conf"]
  Service conf
    <$> getCurrentTime
    <*> initLogs (C.subconfig "log" conf)
    <*> C.require conf "secret"
    <*> initEntropy
    <*> initPasswd
    <*> initDB (C.subconfig "db" conf)
    <*> initStorage (C.subconfig "store" conf)
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
