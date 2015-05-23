{-# LANGUAGE CPP #-}
module Databrary.Web.Service
  ( Web
  , initWeb
  ) where

import Control.Applicative ((<$>))
#ifdef DEVEL
import Control.Concurrent.MVar (newMVar)
#endif
import qualified Data.HashMap.Strict as HM

import Databrary.Web.Types
import Databrary.Web.Files
import Databrary.Web.Cache
#ifdef DEVEL
import Databrary.Web.Rules
#endif

initWeb :: IO Web
initWeb = do
#ifdef DEVEL
  generateWebFiles
#endif
  l <- fmap HM.fromList . mapM (\f -> (,) (webFileRelRaw f) <$> makeWebFileInfo f) =<< allWebFiles
  Web <$>
#ifdef DEVEL
    newMVar
#else
    return
#endif
    l
