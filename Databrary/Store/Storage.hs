{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Storage
  ( Storage(..)
  , StorageM
  , initStorage
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import System.IO.Error (mkIOError, doesNotExistErrorType)
import System.Posix.Files (getFileStatus, isDirectory)

import Control.Has (MonadHas)

data Storage = Storage
  { storageMaster :: FilePath
  , storageFallback :: Maybe FilePath
  , storageUpload :: FilePath
  }

type StorageM c m = (MonadHas Storage c m, MonadIO m)

checkDirectory :: FilePath -> IO FilePath
checkDirectory f = do
  s <- getFileStatus f
  unless (isDirectory s)
    $ ioError $ mkIOError doesNotExistErrorType "storage directory" Nothing (Just f)
  return f

initStorage :: C.Config -> IO Storage
initStorage conf = Storage
  <$> (checkDirectory =<< C.require conf "master")
  <*> C.lookup conf "fallback"
  <*> (checkDirectory =<< C.require conf "upload")
