{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.File
  ( StaticPath
  , staticFilePath
  , staticPath
  , fileResponse
  , serveFile
  , serveStaticFile
  ) where

import Control.Monad (when, mfilter)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAscii, isAlphaNum)
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (ResponseHeaders, hLastModified, hContentType, hIfModifiedSince, notModified304, hIfRange)
import qualified Network.Wai as Wai
import System.Posix.FilePath (joinPath, splitDirectories, (</>))

import Databrary.Ops
import Databrary.Has (peek)
import Databrary.Store
import Databrary.HTTP.Request
import Databrary.HTTP
import qualified Databrary.HTTP.Route as R
import Databrary.Action
import Databrary.Model.Format (Format, getFormatByFilename, unknownFormat, formatMimeType)

newtype StaticPath = StaticPath { staticFilePath :: RawFilePath }

ok :: Char -> Bool
ok '.' = True
ok '-' = True
ok '_' = True
ok c = isAscii c && isAlphaNum c

staticPath :: [BS.ByteString] -> StaticPath
staticPath = StaticPath . joinPath . map component where
  component c
    | not (BS.null c) && BSC.head c /= '.' && BSC.all ok c = c
    | otherwise = error ("staticPath: " ++ BSC.unpack c)

parseStaticPath :: [T.Text] -> Maybe StaticPath
parseStaticPath = fmap (StaticPath . joinPath) . mapM component where
  component c = TE.encodeUtf8 c <? (not (T.null c) && T.head c /= '.' && T.all ok c)

instance R.Routable StaticPath where
  route = R.maybe . parseStaticPath =<< R.path
  toRoute (StaticPath p) = map TE.decodeLatin1 $ splitDirectories p

fileResponse :: (MonadAction c m, MonadIO m) => RawFilePath -> Format -> BS.ByteString -> m (ResponseHeaders, Maybe Wai.FilePart)
fileResponse file fmt etag = do
  (sz, mt) <- fromMaybeM (result =<< notFoundResponse) =<< liftIO (fileInfo file)
  let szi = toInteger sz
      fh = 
        [ ("etag", quoteHTTP etag)
        , (hLastModified, formatHTTPTimestamp mt)
        , (hContentType, formatMimeType fmt)
        -- , (hContentDisposition, ???)
        -- , (hCacheControl, ???)
        ]
  req <- peek
  let ifnm = map unquoteHTTP $ (splitHTTP =<<) $ lookupRequestHeaders "if-none-match" req
      notmod
        | null ifnm = Fold.any (mt <=) $ (parseHTTPTimestamp =<<) $ lookupRequestHeader hIfModifiedSince req
        | otherwise = any (\m -> m == "*" || m == etag) ifnm
  when notmod $ result =<< emptyResponse notModified304 fh
  let ifrng = unquoteHTTP <$> lookupRequestHeader hIfRange req
      part = mfilter (etag /=) ifrng $> -- allow range detection
        Wai.FilePart 0 szi szi -- force full file
  return (fh, part)

serveFile :: (MonadAction c m, MonadIO m) => RawFilePath -> Format -> BS.ByteString -> m Response
serveFile file fmt etag = do
  (fh, part) <- fileResponse file fmt etag
  okResponse fh (file, part)

serveStaticFile :: (MonadAction c m, MonadIO m) => RawFilePath -> StaticPath -> m Response
serveStaticFile dir (StaticPath rel) =
  serveFile (dir </> rel) (fromMaybe unknownFormat $ getFormatByFilename rel) rel
