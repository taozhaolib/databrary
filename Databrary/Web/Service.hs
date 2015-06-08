{-# LANGUAGE CPP #-}
module Databrary.Web.Service
  ( Web
  , initWeb
  ) where

import Control.Applicative ((<$>))
#ifdef DEVEL
import Control.Concurrent.MVar (newMVar)
#endif

import Databrary.Web.Types
#ifdef DEVEL
import Databrary.Web.Rules
#else
import Databrary.Web.Info
#endif

initWeb :: IO Web
initWeb =
  Web <$>
#ifdef DEVEL
    (newMVar =<< generateWebFiles)
#else
    loadWebFileMap
#endif
