{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Storage
  ( Storage(..)
  , StorageM
  , initStorage
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

import Control.Has (MonadHas)

data Storage = Storage
  { storageMaster :: FilePath
  , storageFallback :: Maybe FilePath
  }

type StorageM c m = (MonadHas Storage c m, MonadIO m)

initStorage :: C.Config -> IO Storage
initStorage conf = do
  master <- C.require conf "master"
  fallback <- C.lookup conf "fallback"
  return $ Storage
    { storageMaster = master
    , storageFallback = fallback
    }
