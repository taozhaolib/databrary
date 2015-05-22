{-# LANGUAGE CPP #-}
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
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromJust)

import Databrary.Has (peeks)
import Databrary.Store
import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Rules

makeWebFileInfo :: WebFilePath -> IO WebFileInfo
makeWebFileInfo f = WebFileInfo f
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
