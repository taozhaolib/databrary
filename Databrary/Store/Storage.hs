{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Storage
  ( Storage(..)
  , MonadStorage
  , initStorage
  ) where

import qualified Data.ByteString.Char8 as BSC
import Control.Applicative ((<$>) )
import Control.Monad (unless, foldM_)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Foldable as Fold
import System.Directory (getTemporaryDirectory)
import System.IO.Error (mkIOError, doesNotExistErrorType, illegalOperationErrorType)
import System.Posix.Files.ByteString (getFileStatus, isDirectory, deviceID)

import Databrary.Has (MonadHas)
import Databrary.Store

data Storage = Storage
  { storageMaster :: RawFilePath
  , storageFallback :: Maybe RawFilePath
  , storageTemp :: RawFilePath
  , storageUpload :: RawFilePath
  }

type MonadStorage c m = (MonadHas Storage c m, MonadIO m)

initStorage :: C.Config -> IO Storage
initStorage conf = do
  master <- C.require conf "master"
  fallback <- C.lookup conf "fallback"
  temp <- maybe (rawFilePath <$> getTemporaryDirectory) return =<< C.lookup conf "temp"
  upload <- C.require conf "upload"

  foldM_ (\dev f -> do
    s <- getFileStatus f
    unless (isDirectory s)
      $ ioError $ mkIOError doesNotExistErrorType "storage directory" Nothing (Just (BSC.unpack f))
    let d = deviceID s
    unless (Fold.all (d ==) dev)
      $ ioError $ mkIOError illegalOperationErrorType "storage filesystem" Nothing (Just (BSC.unpack f))
    return $ Just d)
    Nothing [master, temp, upload]

  return $ Storage master fallback temp upload
