{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.File
  ( StaticPath
  , serveFile
  , serveStaticFile
  ) where

import Control.Applicative ((<$), (<$>))
import Control.Monad (guard, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAscii, isAlphaNum)
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Network.HTTP.Types (hLastModified, hContentType, hIfModifiedSince, notModified304, hIfRange)
import qualified Network.Wai as Wai
import System.FilePath (joinPath, splitDirectories, (</>))
import System.IO.Error (tryIOError)
import System.Posix.Files (getFileStatus, modificationTimeHiRes, fileSize)

import Databrary.Action
import Databrary.Web.HTTP
import qualified Databrary.Web.Route as R
import Databrary.Model.Format (Format, getFormatByFilename, unknownFormat, formatMimeType)

newtype StaticPath = StaticPath FilePath

staticPath :: [T.Text] -> Maybe StaticPath
staticPath p = StaticPath . joinPath <$> mapM component p where
  component c = T.unpack c <$ guard (not (T.null c) && T.head c /= '.' && T.all ok c)
  ok '.' = True
  ok '-' = True
  ok '_' = True
  ok c = isAscii c && isAlphaNum c

instance R.Routable StaticPath where
  route = R.maybe . staticPath =<< R.path
  toRoute (StaticPath p) = map T.pack $ splitDirectories p

serveFile :: (ActionM c m, MonadIO m) => FilePath -> Format -> BS.ByteString -> m Response
serveFile file fmt etag = do
  minfo <- liftIO $ tryIOError $ getFileStatus file
  info <- either (\_ -> result =<< notFoundResponse) return minfo
  let mt = posixSecondsToUTCTime $ modificationTimeHiRes info
      fh = 
        [ ("etag", quoteHTTP etag)
        , (hLastModified, formatHTTPTimestamp mt)
        , (hContentType, formatMimeType fmt)
        -- , (hContentDisposition, ???)
        -- , (hCacheControl, ???)
        ]
      sz = toInteger $ fileSize info
  ifnm <- map unquoteHTTP . (splitHTTP =<<) <$> getRequestHeaders "if-none-match"
  notmod <- if null ifnm
    then Fold.any (mt <=) . (parseHTTPTimestamp =<<) <$> getRequestHeader hIfModifiedSince
    else return $ any (\m -> m == "*" || m == etag) ifnm
  when notmod $ result =<< emptyResponse notModified304 fh
  ifrng <- fmap unquoteHTTP <$> getRequestHeader hIfRange
  let part = if Fold.any (etag /=) ifrng
        then Nothing -- allow range detection
        else Just $ Wai.FilePart 0 sz sz -- force full file
  okResponse fh (file, part)

serveStaticFile :: (ActionM c m, MonadIO m) => FilePath -> StaticPath -> m Response
serveStaticFile dir (StaticPath rel) =
  serveFile (dir </> rel) (fromMaybe unknownFormat $ getFormatByFilename rel) (BSC.pack rel)
