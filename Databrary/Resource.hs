{-# LANGUAGE OverloadedStrings #-}
module Databrary.Resource
  ( Resource(..)
  , ResourceM
  , getResource
  , ResourceT
  , initResource
  , runResource
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Control.Has (MonadHas, peeks)
import Databrary.Resource.DB
import Databrary.Resource.Entropy

data Resource = Resource
  { resourceConfig :: C.Config
  , resourceSecret :: BS.ByteString
  , resourceDB :: DBConn
  , resourceEntropy :: Entropy
  }

type ResourceM c m = MonadHas Resource c m

getResource :: ResourceM c m => (Resource -> a) -> m a
getResource = peeks

initResource :: IO Resource
initResource = do
  conf <- C.load [C.Required "databrary.conf"]
  Resource conf
    <$> C.require conf "secret"
    <*> initDB (C.subconfig "db" conf)
    <*> initEntropy

type ResourceT = ReaderT Resource

runResource :: ResourceT m a -> Resource -> m a
runResource = runReaderT
