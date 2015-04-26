{-# LANGUAGE OverloadedStrings, CPP #-}
module Databrary.Controller.Web
  ( staticPath
  , webFile
  ) where

import Crypto.Hash (digestToHexByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAscii, isAlphaNum)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Posix.FilePath (joinPath, splitDirectories, (</>))

import Databrary.Ops
import Databrary.Store
import Databrary.Model.Format (getFormatByFilename, unknownFormat)
import Databrary.Action.Route
import Databrary.Action
import Databrary.HTTP.File
import qualified Databrary.HTTP.Route as R
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Cache

newtype StaticPath = StaticPath RawFilePath

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

webFile :: StaticPath -> AppRAction
webFile sp@(StaticPath p) = action GET ("public" :: T.Text, sp) $ do
  wf <- maybeAction =<< lookupWebFile p
  let f = webDir </> p
  serveFile f (fromMaybe unknownFormat $ getFormatByFilename p) Nothing (digestToHexByteString $ webFileTag wf)
