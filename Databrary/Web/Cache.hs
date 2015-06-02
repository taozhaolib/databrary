{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}
module Databrary.Web.Cache
  ( lookupWebFile
  ) where

import Databrary.Files
import Databrary.Web
import Databrary.Web.Types

#ifdef DEVEL
import Control.Applicative ((<$>))
import Control.Arrow (first, right)
import Control.Concurrent.MVar (modifyMVar)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Tuple (swap)

import Databrary.Web.Rules

#else
import qualified Data.HashMap.Strict as HM

#endif

lookupWebFile :: RawFilePath -> Web -> IO (Either String (WebFilePath, WebFileInfo))
lookupWebFile f (Web wc) =
#ifdef DEVEL
  modifyMVar wc $ \wm -> do
    swap . first (right (wf, )) <$>
      runStateT (runExceptT $ generateWebFile False wf) wm
  where wf = fromRawFilePath f
#else
  return $ HM.lookup f wc
#endif
