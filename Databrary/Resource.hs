{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Databrary.Resource
  ( Resource(..)
  , MonadHasResource
  , getResource
  , initResource
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Control.Has (makeHasRec, peeks)
import Databrary.DB
import Databrary.Entropy

data Resource = Resource
  { resourceConfig :: C.Config
  , resourceSecret :: BS.ByteString
  , resourceDB :: DBConn
  , resourceEntropy :: Entropy
  }

makeHasRec ''Resource ['resourceConfig, 'resourceDB, 'resourceEntropy]

getResource :: MonadHasResource c m => (Resource -> a) -> m a
getResource = peeks

initResource :: IO Resource
initResource = do
  conf <- C.load [C.Required "databrary.conf"]
  Resource conf
    <$> C.require conf "secret"
    <*> initDB (C.subconfig "db" conf)
    <*> initEntropy
