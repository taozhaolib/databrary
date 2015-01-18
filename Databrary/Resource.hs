{-# LANGUAGE OverloadedStrings, FlexibleInstances, FlexibleContexts, TypeSynonymInstances, DefaultSignatures, TypeFamilies #-}
module Databrary.Resource
  ( Resource(..)
  , HasResource(..)
  , ResourceM(..)
  , ResourceT
  , initResource
  , runResource
  ) where

import Control.Monad (liftM3)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.RWS.Strict (RWST)
import qualified Data.ByteString as BS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Monoid (Monoid)

import Databrary.Resource.DB
import Databrary.Resource.Entropy

data Resource = Resource
  { resourceConfig :: C.Config
  , resourceSecret :: BS.ByteString
  , resourceDB :: DBConn
  , resourceEntropy :: Entropy
  }

class HasResource r where
  toResource :: r -> Resource

instance HasResource Resource where
  toResource = id

class Monad m => ResourceM m where
  getResource :: (Resource -> a) -> m a
  default getResource :: (MonadReader r m, HasResource r) => (Resource -> a) -> m a
  getResource f = asks (f . toResource)

instance (Monad m, HasResource r) => ResourceM (ReaderT r m)
instance (Monad m, Monoid w, HasResource r) => ResourceM (RWST r w s m)

initResource :: IO Resource
initResource = do
  conf <- C.load [C.Required "databrary.conf"]
  liftM3 (Resource conf)
    (C.require conf "secret")
    (initDB $ C.subconfig "db" conf)
    initEntropy

type ResourceT = ReaderT Resource

runResource :: ResourceT m a -> Resource -> m a
runResource = runReaderT
