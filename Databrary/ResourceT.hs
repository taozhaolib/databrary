module Databrary.ResourceT
  ( MonadResourceT
  , liftResourceT
  ) where

import Control.Monad.Trans.Resource (InternalState, ResourceT, runInternalState)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Databrary.Has (MonadHas, peek)

type MonadResourceT c m = (MonadIO m, MonadHas InternalState c m)

liftResourceT :: MonadResourceT c m => ResourceT IO a -> m a
liftResourceT r = liftIO . runInternalState r =<< peek
