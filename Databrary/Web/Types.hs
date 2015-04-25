{-# LANGUAGE CPP #-}
module Databrary.Web.Types
  ( WebFile(..)
  , Web(..)
  , MonadWeb
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (MonadIO)
import Crypto.Hash (Digest, MD5)
import qualified Data.HashMap.Strict as HM
import System.Posix.ByteString.FilePath (RawFilePath)

import Databrary.Has (MonadHas)
import Databrary.Model.Time

data WebFile = WebFile
  { webFileTag :: !(Digest MD5)
  , webFileTimestamp :: !Timestamp
  }

data Web = Web
  { webCache ::
#ifdef DEVEL
    MVar
#endif
      (HM.HashMap RawFilePath WebFile)
  }

type MonadWeb c m = (MonadHas Web c m, MonadIO m)

