{-# LANGUAGE OverloadedStrings, CPP #-}
module Databrary.Controller.Web
  ( staticPath
  , webFile
  ) where

import Control.Concurrent.MVar (MVar, newMVar, readMVar, modifyMVar_)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Crypto.Hash (Digest, MD5, digestToHexByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAscii, isAlphaNum)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.FilePath (joinPath, splitDirectories, (</>))

import Databrary.Ops
import Databrary.Store
import Databrary.Model.Format (getFormatByFilename, unknownFormat)
import Databrary.Action.Route
import Databrary.Action
import Databrary.HTTP.File
import qualified Databrary.HTTP.Route as R
import Databrary.Web.Files
import Databrary.Web.Rules

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

webCache :: MVar (HM.HashMap RawFilePath (Bool, Digest MD5))
webCache = unsafePerformIO $ newMVar HM.empty

webFile :: StaticPath -> AppRAction
webFile sp@(StaticPath p) = action GET ("public" :: T.Text, sp) $ do
#ifdef DEVEL
  g <- generateWebFile p
  let d = isJust g
#else
  (d, h) <- maybeAction . HM.lookup p =<< readMVar webCache
#endif
  let f = (if d then genDir else webDir) </> p
#ifdef DEVEL
      rehash = do
        h <- hashFile f
        modifyMVar_ webCache (return . HM.insert p (d, h))
        return h
  h <- liftIO $ case g of
    Just True -> rehash
    _ -> do
      c <- readMVar webCache
      fromMaybeM rehash $ do
        (cd, ch) <- HM.lookup p c
        guard (cd == d)
        return ch
#endif
  serveFile f (fromMaybe unknownFormat $ getFormatByFilename p) Nothing (digestToHexByteString h)
