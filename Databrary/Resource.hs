{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Databrary.Resource
  ( Resource(..)
  , MonadHasResource
  , initResource
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Control.Has (makeHasRec)
import Databrary.DB (DBConn, initDB)
import Databrary.Entropy (Entropy, initEntropy)
import Databrary.Web.Client (HTTPClient, initHTTPClient)
import Databrary.Store.Storage (Storage, initStorage)

data Resource = Resource
  { resourceConfig :: C.Config
  , resourceSecret :: BS.ByteString
  , resourceDB :: DBConn
  , resourceEntropy :: Entropy
  , resourceHTTPClient :: HTTPClient
  , resourceStorage :: Storage
  }

makeHasRec ''Resource ['resourceConfig, 'resourceDB, 'resourceEntropy, 'resourceHTTPClient, 'resourceStorage]

initResource :: IO Resource
initResource = do
  conf <- C.load [C.Required "databrary.conf"]
  Resource conf
    <$> C.require conf "secret"
    <*> initDB (C.subconfig "db" conf)
    <*> initEntropy
    <*> initHTTPClient
    <*> initStorage (C.subconfig "store" conf)
