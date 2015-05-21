{-# LANGUAGE CPP #-}
module Databrary.Web.Types
  ( WebFile(..)
  , Web(..)
  , MonadWeb
  , WebGenerator
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Maybe (MaybeT)
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

type WebGenerator = RawFilePath -> Maybe Timestamp -> MaybeT IO Bool
