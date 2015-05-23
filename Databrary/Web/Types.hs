{-# LANGUAGE CPP #-}
module Databrary.Web.Types
  ( WebFileInfo(..)
  , Web(..)
  , MonadWeb
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (MonadIO)
import Crypto.Hash (Digest, MD5)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import System.Posix.ByteString.FilePath (RawFilePath)

import Databrary.Has (MonadHas)
import Databrary.Model.Time
import Databrary.Web.Files (WebFilePath)

data WebFileInfo = WebFileInfo
  { webFilePath :: !WebFilePath
  , webFileFormat :: !BS.ByteString
  , webFileTag :: !(Digest MD5)
  , webFileTimestamp :: !Timestamp
  }

data Web = Web
  { webCache ::
#ifdef DEVEL
    MVar
#endif
      (HM.HashMap RawFilePath WebFileInfo)
  }

type MonadWeb c m = (MonadHas Web c m, MonadIO m)
