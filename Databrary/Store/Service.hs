{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Service
  ( Storage
  , initStorage
  ) where

import Control.Monad (unless, foldM_)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Foldable as Fold
import System.Directory (getTemporaryDirectory, createDirectoryIfMissing)
import System.IO.Error (mkIOError, doesNotExistErrorType, illegalOperationErrorType)
import System.Posix.FilePath (addTrailingPathSeparator)
import System.Posix.Files.ByteString (isDirectory, deviceID)

import Databrary.Ops
import Databrary.Files
import Databrary.Store.Types
import Databrary.Store.Transcoder

initStorage :: C.Config -> IO Storage
initStorage conf = do
  master <- C.require conf "master"
  fallback <- C.lookup conf "fallback"
  temp <- fromMaybeM (toRawFilePath <$> getTemporaryDirectory) =<< C.lookup conf "temp"
  upload <- C.require conf "upload"
  cache <- C.lookup conf "cache"

  foldM_ (\dev f -> do
    s <- getFileStatus f
    unless (isDirectory s)
      $ ioError $ mkIOError doesNotExistErrorType "storage directory" Nothing (Just (toFilePath f))
    let d = deviceID s
    unless (Fold.all (d ==) dev)
      $ ioError $ mkIOError illegalOperationErrorType "storage filesystem" Nothing (Just (toFilePath f))
    return $ Just d)
    Nothing $ [master, temp, upload]

  Fold.mapM_ (\c -> createDirectoryIfMissing False (toFilePath c </> "tmp")) cache

  transcodeHost <- C.lookup conf "transcode.host"
  transcodeDir <- C.lookup conf "transcode.dir"
  tc <- initTranscoder transcodeHost transcodeDir

  return $ Storage
    { storageMaster = master
    , storageFallback = fallback
    , storageTemp = addTrailingPathSeparator temp
    , storageUpload = upload
    , storageCache = cache
    , storageTranscoder = tc
    }
