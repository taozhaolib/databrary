{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Storage
  ( Storage(..)
  , StorageM
  , initStorage
  ) where

import qualified Data.ByteString.Char8 as BSC
import Control.Applicative ((<$>), (<*>))
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import System.Directory (getTemporaryDirectory)
import System.IO.Error (mkIOError, doesNotExistErrorType)
import System.Posix.Files.ByteString (getFileStatus, isDirectory)

import Control.Has (MonadHas)
import Databrary.Store

data Storage = Storage
  { storageMaster :: RawFilePath
  , storageFallback :: Maybe RawFilePath
  , storageTemp :: RawFilePath
  , storageUpload :: RawFilePath
  }

type StorageM c m = (MonadHas Storage c m, MonadIO m)

checkDirectory :: RawFilePath -> IO RawFilePath
checkDirectory f = do
  s <- getFileStatus f
  unless (isDirectory s)
    $ ioError $ mkIOError doesNotExistErrorType "storage directory" Nothing (Just (BSC.unpack f))
  return f

initStorage :: C.Config -> IO Storage
initStorage conf = Storage
  <$> (checkDirectory =<< C.require conf "master")
  <*> C.lookup conf "fallback"
  <*> (maybe (rawFilePath <$> getTemporaryDirectory) return =<< C.lookup conf "temp")
  <*> (checkDirectory =<< C.require conf "upload")
