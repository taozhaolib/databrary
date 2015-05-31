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
import Databrary.Web.Rules

initWeb :: IO Web
initWeb = do
  m <- generateWebFiles
  Web <$>
#ifdef DEVEL
    newMVar
#else
    return
#endif
    m
