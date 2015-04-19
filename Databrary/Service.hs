{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Databrary.Service
  ( Service(..)
  , MonadHasService
  , initService
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Time.Clock (getCurrentTime)

import Databrary.Has (makeHasRec)
import Databrary.Service.DB (DBConn, initDB)
import Databrary.Service.Entropy (Entropy, initEntropy)
import Databrary.HTTP.Client (HTTPClient, initHTTPClient)
import Databrary.Store.Service (Storage, initStorage)
import Databrary.Service.Passwd (Passwd, initPasswd)
import Databrary.Media.AV (AV, initAV)
import Databrary.Model.Time

data Service = Service
  { serviceConfig :: !C.Config
  , serviceStartTime :: !Timestamp
  , serviceSecret :: !BS.ByteString
  , serviceDB :: !DBConn
  , serviceEntropy :: !Entropy
  , serviceHTTPClient :: !HTTPClient
  , serviceStorage :: !Storage
  , servicePasswd :: !Passwd
  , serviceAV :: !AV
  }

makeHasRec ''Service ['serviceConfig, 'serviceDB, 'serviceEntropy, 'serviceHTTPClient, 'serviceStorage, 'servicePasswd, 'serviceAV]

initService :: IO Service
initService = do
  conf <- C.load [C.Required "databrary.conf"]
  Service conf
    <$> getCurrentTime
    <*> C.require conf "secret"
    <*> initDB (C.subconfig "db" conf)
    <*> initEntropy
    <*> initHTTPClient
    <*> initStorage (C.subconfig "store" conf)
    <*> initPasswd
    <*> initAV
