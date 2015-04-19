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

import Databrary.Has (makeHasRec)
import Databrary.DB (DBConn, initDB)
import Databrary.Entropy (Entropy, initEntropy)
import Databrary.Web.Client (HTTPClient, initHTTPClient)
import Databrary.Store.Storage (Storage, initStorage)
import Databrary.Passwd (Passwd, initPasswd)
import Databrary.Media.AV (AV, initAV)

data Service = Service
  { serviceConfig :: C.Config
  , serviceSecret :: BS.ByteString
  , serviceDB :: DBConn
  , serviceEntropy :: Entropy
  , serviceHTTPClient :: HTTPClient
  , serviceStorage :: Storage
  , servicePasswd :: Passwd
  , serviceAV :: AV
  }

makeHasRec ''Service ['serviceConfig, 'serviceDB, 'serviceEntropy, 'serviceHTTPClient, 'serviceStorage, 'servicePasswd, 'serviceAV]

initService :: IO Service
initService = do
  conf <- C.load [C.Required "databrary.conf"]
  Service conf
    <$> C.require conf "secret"
    <*> initDB (C.subconfig "db" conf)
    <*> initEntropy
    <*> initHTTPClient
    <*> initStorage (C.subconfig "store" conf)
    <*> initPasswd
    <*> initAV
