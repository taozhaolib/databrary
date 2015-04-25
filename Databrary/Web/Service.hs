{-# LANGUAGE CPP #-}
module Databrary.Web.Service
  ( Web
  , initWeb
  ) where

import Control.Applicative ((<$>))
#ifdef DEVEL
import Control.Concurrent.MVar (newMVar)
#endif
import Control.Monad (liftM2)
import qualified Data.HashMap.Strict as HM

import Databrary.Web.Types
import Databrary.Web.Cache
import Databrary.Web.Rules

initWeb :: IO Web
initWeb = do
  l <- fmap HM.fromList . mapM (liftM2 fmap (,) makeWebFile) =<< generateWebFiles
#ifdef DEVEL
  Web <$> newMVar l
#else
  return $ Web l
#endif
