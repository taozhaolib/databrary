module Databrary.Web.HTTP
  ( splitHTTP
  , unquoteHTTP
  , httpTimestamp
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isSpace)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C.Types (CTime(..))
import Network.HTTP.Date (formatHTTPDate, epochTimeToHTTPDate)

import Databrary.Types.Time

splitHTTP :: BS.ByteString -> [BS.ByteString]
splitHTTP = filter (not . BS.null) . map trim . BSC.split ',' where
  trim = fst . BSC.spanEnd isSpace . BSC.dropWhile isSpace

unquoteHTTP :: BS.ByteString -> BS.ByteString
unquoteHTTP s
  | BS.length s >= 2 && BSC.head s == '"' && BSC.last s == '"' =
    BSC.pack $ unquote $ BSC.unpack $ BS.tail $ BS.init s
  | otherwise = s where
    unquote ('\\':c:r) = c:unquote r
    unquote (c:r) = c:unquote r
    unquote [] = []

httpTimestamp :: Timestamp -> BS.ByteString
httpTimestamp = formatHTTPDate . epochTimeToHTTPDate . CTime . round . utcTimeToPOSIXSeconds
