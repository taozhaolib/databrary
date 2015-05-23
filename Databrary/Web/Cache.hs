{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}
module Databrary.Web.Cache
  ( makeWebFileInfo
  , lookupWebFile
  ) where

import Control.Applicative ((<$>), (<*>))
#ifdef DEVEL
import Control.Concurrent.MVar (modifyMVar)
#endif
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, fromJust)
import System.FilePath (takeExtension)

import Databrary.Has (peeks)
import Databrary.Store
import Databrary.Model.Format
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Rules

staticFormats :: [(String, BS.ByteString)]
staticFormats = concatMap (\f -> map (\e -> ('.':BSC.unpack e, formatMimeType f)) $ formatExtension f) allFormats ++
  [ (".html", "text/html")
  , (".js", "application/javascript")
  , (".css", "text/css")
  , (".svg", "image/svg+xml")
  ]

makeWebFileInfo :: WebFilePath -> IO WebFileInfo
makeWebFileInfo f = WebFileInfo f
  (fromMaybe "application/octet-stream" $ lookup (takeExtension $ webFileRel f) staticFormats)
  <$> hashFile (webFileAbsRaw f)
  <*> (fromJust <$> runMaybeT (webFileTime f))

lookupWebFile :: MonadWeb c m => RawFilePath -> m (Maybe WebFileInfo)
lookupWebFile f = do
  wc <- peeks webCache
#ifdef DEVEL
  liftIO $ modifyMVar wc $ \wm -> do
    let wf = HM.lookup f wm
        wfp = webFileRaw f
    maybe
      (return (HM.delete f wm, Nothing))
      (\r -> if r
        then do
          wf' <- makeWebFileInfo wfp
          return (HM.insert f wf' wm, Just wf')
        else
          return (wm, wf))
      =<< runMaybeT (generateWebFile wfp (webFileTimestamp <$> wf))
#else
  return $ HM.lookup f wc
#endif
