{-# LANGUAGE CPP #-}
module Databrary.Web.Cache
  ( makeWebFile
  , lookupWebFile
  ) where

import Control.Applicative ((<$>), (<*>))
#ifdef DEVEL
import Control.Concurrent.MVar (modifyMVar)
#endif
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)
import System.Posix.FilePath ((</>))

import Databrary.Has (peeks)
import Databrary.Store
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Rules

makeWebFile :: RawFilePath -> IO WebFile
makeWebFile f = WebFile
  <$> hashFile wf
  <*> (snd . fromJust <$> fileInfo wf)
  where wf = webDir </> f

lookupWebFile :: MonadWeb c m => RawFilePath -> m (Maybe WebFile)
lookupWebFile f = do
  wc <- peeks webCache
#ifdef DEVEL
  liftIO $ modifyMVar wc $ \wm ->
    let wf = HM.lookup f wm in
    maybe
      (return (HM.delete f wm, Nothing))
      (\r -> if r
        then do
          wf' <- makeWebFile f
          return (HM.insert f wf' wm, Just wf')
        else
          return (wm, wf))
      =<< generateWebFile f (webFileTimestamp <$> wf)
#else
  return $ HM.lookup f wc
#endif
