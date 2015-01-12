{-# LANGUAGE TemplateHaskell #-}
module Databrary.Resource
  ( Resource
  , ResourceT
  , initResource
  , runResource
  ) where

import Control.Lens (makeLenses)
import Control.Monad (liftM2)
import Control.Monad.Reader (ReaderT, runReaderT)

import Databrary.Resource.DB
import Databrary.Resource.Entropy

data Resource = Resource
  { _db :: DBConn
  , _entropy :: Entropy
  }

makeLenses ''Resource

instance HasDB Resource where
  dbLens = db
instance HasEntropy Resource where
  entropyLens = entropy

initResource :: IO Resource
initResource = liftM2 Resource initDB initEntropy

type ResourceT = ReaderT Resource

runResource :: ResourceT m a -> Resource -> m a
runResource = runReaderT
