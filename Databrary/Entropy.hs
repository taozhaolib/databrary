{-# LANGUAGE FlexibleInstances, FlexibleContexts, ConstraintKinds #-}
module Databrary.Entropy
  ( EntropyM
  , entropyBytes
  , entropyBytesGenerator
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified System.Entropy as Entropy

import Databrary.Resource.Entropy
import Databrary.Resource

type EntropyM c m = (MonadIO m, ResourceM c m)

entropyBytesGenerator :: EntropyM c m => Int -> m (IO ByteString)
entropyBytesGenerator n = do
  e <- getResource resourceEntropy
  liftIO $ withEntropy e (\h -> return $ Entropy.hGetEntropy h n)

entropyBytes :: EntropyM c m => Int -> m ByteString
entropyBytes n = liftIO =<< entropyBytesGenerator n
