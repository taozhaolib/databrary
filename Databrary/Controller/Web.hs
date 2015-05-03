{-# LANGUAGE OverloadedStrings, CPP #-}
module Databrary.Controller.Web
  ( staticPath
  , webFile
  , formatIcon
  ) where

import Crypto.Hash (digestToHexByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAscii, isAlphaNum)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Posix.FilePath (joinPath, splitFileName, splitDirectories, splitExtension, (</>))

import Databrary.Iso.Types (invMap)
import Databrary.Ops
import Databrary.Store
import Databrary.Model.Format
import Databrary.Action.Route
import Databrary.Action
import Databrary.HTTP.File
import Databrary.HTTP.Route.PathParser (PathParser(..), (>/>))
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Cache

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

pathStatic :: PathParser (Maybe StaticPath)
pathStatic = invMap parseStaticPath (maybe [] $ map TE.decodeLatin1 . splitDirectories . staticFilePath) PathAll

webFile :: AppRoute (Maybe StaticPath)
webFile = action GET ("public" >/> pathStatic) $ \sp -> do
  StaticPath p <- maybeAction sp
  wf <- maybeAction =<< lookupWebFile p
  let f = webDir </> p
  serveFile f (fromMaybe unknownFormat $ getFormatByFilename p) Nothing (digestToHexByteString $ webFileTag wf)

formatIcon :: AppRoute Format
formatIcon = invMap pf fp webFile where
  fp f = Just $ staticPath
    [ "images", "filetype"
    , case formatExtension f of { e:_ -> e ; _ -> "_blank" } <> ".svg"
    ]
  pf (Just (StaticPath p))
    | ("images/filetype/", i) <- splitFileName p
    , (e, ".svg") <- splitExtension i
    , Just f <- getFormatByExtension e = f
  pf _ = unknownFormat
