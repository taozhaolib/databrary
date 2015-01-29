module Databrary.Web.File
  (
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import System.Posix.Files (getFileStatus, FileStatus)
import System.IO.Error (catchIOError)

import Databrary.Action

serveFile :: Has Request q => BS.ByteString -> FilePath -> Action q r
serveFile etag file = do
  info <- catchIOError (getFileStatus file) $
    result notFoundResult
  ifnm <- map unquoteHttp . concatMap splitHttp <$> getRequestHeaderList "if-none-match"
  when (any (\m -> m == "*" || m == etag) ifnm) $
    resultIO notModified304 []
  respond 
