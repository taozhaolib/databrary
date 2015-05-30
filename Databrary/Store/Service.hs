{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Service
  ( Storage
  , initStorage
  ) where

import qualified Data.ByteString.Char8 as BSC
import Control.Applicative ((<$>) )
import Control.Monad (unless, foldM_)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Foldable as Fold
import System.Directory (getTemporaryDirectory)
import System.IO.Error (mkIOError, doesNotExistErrorType, illegalOperationErrorType)
import System.Posix.FilePath (addTrailingPathSeparator)
import System.Posix.Files.ByteString (isDirectory, deviceID)

import Databrary.Files
import Databrary.Store.Types
import Databrary.Store.Transcoder

initStorage :: C.Config -> IO Storage
initStorage conf = do
  master <- C.require conf "master"
  fallback <- C.lookup conf "fallback"
  temp <- maybe (toRawFilePath <$> getTemporaryDirectory) return =<< C.lookup conf "temp"
  upload <- C.require conf "upload"

  foldM_ (\dev f -> do
    s <- getFileStatus f
    unless (isDirectory s)
      $ ioError $ mkIOError doesNotExistErrorType "storage directory" Nothing (Just (BSC.unpack f))
    let d = deviceID s
    unless (Fold.all (d ==) dev)
      $ ioError $ mkIOError illegalOperationErrorType "storage filesystem" Nothing (Just (BSC.unpack f))
    return $ Just d)
    Nothing $ [master, temp, upload]

  transcodeHost <- C.lookup conf "transcode.host"
  transcodeDir <- C.lookup conf "transcode.dir"
  tc <- initTranscoder transcodeHost transcodeDir

  return $ Storage
    { storageMaster = master
    , storageFallback = fallback
    , storageTemp = addTrailingPathSeparator temp
    , storageUpload = upload
    , storageTranscoder = tc
    }
