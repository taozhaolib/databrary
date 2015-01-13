{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeSynonymInstances, DefaultSignatures, TypeFamilies #-}
module Databrary.Resource
  ( Resource
  , resourceDB
  , resourceEntropy
  , ResourceT
  , HasResource(..)
  , initResource
  , runResource
  ) where

import Control.Monad (liftM2)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (MonadTrans, lift)

import Databrary.Resource.DB
import Databrary.Resource.Entropy
import Databrary.Wai (WaiT)

data Resource = Resource
  { resourceDB :: DBConn
  , resourceEntropy :: Entropy
  }

initResource :: IO Resource
initResource = liftM2 Resource initDB initEntropy

type ResourceT = ReaderT Resource

class Monad m => HasResource m where
  getResource :: (Resource -> a) -> m a
  default getResource :: (MonadTrans t, HasResource b, m ~ t b) => (Resource -> a) -> t b a
  getResource = lift . getResource

instance Monad m => HasResource (ResourceT m) where
  getResource = asks
instance HasResource m => HasResource (WaiT r m)

runResource :: ResourceT m a -> Resource -> m a
runResource = runReaderT
