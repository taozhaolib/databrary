module Databrary.Store
  ( RawFilePath
  , rawFilePath
  , unRawFilePath
  ) where

import qualified Data.ByteString.Char8 as BSC
import System.Posix.FilePath (RawFilePath)

rawFilePath :: FilePath -> RawFilePath
rawFilePath = BSC.pack

unRawFilePath :: RawFilePath -> FilePath
unRawFilePath = BSC.unpack
