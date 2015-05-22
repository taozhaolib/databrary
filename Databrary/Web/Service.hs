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
import Databrary.Web.Rules

initWeb :: IO Web
initWeb = do
  l <- fmap HM.fromList . mapM (\f -> (,) (webFileRelRaw f) <$> makeWebFileInfo f) =<< generateWebFiles
#ifdef DEVEL
  Web <$> newMVar l
#else
  return $ Web l
#endif
