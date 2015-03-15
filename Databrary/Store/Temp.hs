{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.Temp
  ( TempFile(..)
  , makeTempFile
  , releaseTempFile
  , renameTempFile
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ReleaseKey, allocate, release, unprotect)
import System.IO (Handle)
import System.Posix.FilePath (RawFilePath, (</>))
import System.Posix.Files.ByteString (removeLink, rename)
import System.Posix.Temp.ByteString (mkstemp)

import Databrary.Has (peeks)
import Databrary.Resource (MonadResourceT, liftResourceT)
import Databrary.Store.Storage

data TempFile = TempFile
  { tempFileRelease :: ReleaseKey
  , tempFilePath :: RawFilePath
  }

makeTempFile :: (MonadResourceT c m, MonadStorage c m) => m (TempFile, Handle)
makeTempFile = do
  d <- peeks storageTemp
  (k, (f, h)) <- liftResourceT $ allocate (mkstemp (d </> "XXXXXX")) (removeLink . fst)
  return (TempFile k f, h)

releaseTempFile :: MonadResourceT c m => TempFile -> m ()
releaseTempFile = liftResourceT . release . tempFileRelease

renameTempFile :: MonadResourceT c m => TempFile -> RawFilePath -> m ()
renameTempFile (TempFile k f) t = do
  liftIO $ rename f t
  void $ liftResourceT $ unprotect k
