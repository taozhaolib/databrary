{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}
module Databrary.Web.Cache
  ( lookupWebFile
  ) where

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types

#ifdef DEVEL
import Control.Applicative ((<$>))
import Control.Concurrent.MVar (modifyMVar)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)

import Databrary.Web.Rules

#else
import qualified Data.HashMap.Strict as HM

#endif

lookupWebFile :: RawFilePath -> Web -> IO (Maybe (WebFilePath, WebFileInfo))
lookupWebFile f (Web wc) =
#ifdef DEVEL
  modifyMVar wc $ \wm -> do
    either (const (wm, Nothing)) (\(i, m) -> (m, Just (wf, i))) <$>
      runExceptT (runStateT (generateWebFile wf) wm)
  where wf = fromRawFilePath f
#else
  return $ HM.lookup f wc
#endif
