module Databrary.Store
  ( unRawFilePath
  ) where

import qualified Data.ByteString.Char8 as BSC
import System.Posix.FilePath (RawFilePath)

unRawFilePath :: RawFilePath -> FilePath
unRawFilePath = BSC.unpack
