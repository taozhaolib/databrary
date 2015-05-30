{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}
module Databrary.Web.Cache
  ( makeWebFileInfo
  , lookupWebFile
  ) where

import Control.Applicative ((<$>), (<*>))
#ifdef DEVEL
import Control.Concurrent.MVar (modifyMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..))
#endif
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import System.FilePath (takeExtension)

import Databrary.Has (peeks)
import Databrary.Files
import Databrary.Model.Format
import Databrary.Web
import Databrary.Web.Types
#ifdef DEVEL
import Databrary.Web.Rules
#endif

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
  <*> (modificationTimestamp <$> getFileStatus f)

lookupWebFile :: MonadWeb c m => RawFilePath -> m (Maybe WebFileInfo)
lookupWebFile f = do
  wc <- peeks webCache
#ifdef DEVEL
  liftIO $ modifyMVar wc $ \wm -> do
    let wf = HM.lookup f wm
        wfp = fromRawFilePath f
    maybe
      (return (HM.delete f wm, Nothing))
      (\r -> if r
        then do
          wf' <- makeWebFileInfo wfp
          return (HM.insert f wf' wm, Just wf')
        else
          return (wm, wf))
      =<< runMaybeT (runReaderT (generateWebFile wfp) (webFileTimestamp <$> wf))
#else
  return $ HM.lookup f wc
#endif
