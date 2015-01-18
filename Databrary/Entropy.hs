{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Databrary.Entropy
  ( EntropyM
  , entropyBytes
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified System.Entropy as Entropy

import Databrary.Resource.Entropy
import Databrary.Resource

class Monad m => EntropyM m where
  liftEntropy :: (Entropy.CryptHandle -> IO a) -> m a

instance (MonadIO m, ResourceM m) => EntropyM m where
  liftEntropy f = do
    e <- getResource resourceEntropy
    liftIO $ withEntropy e f

entropyBytes :: EntropyM m => Int -> m ByteString
entropyBytes n =
  liftEntropy (\h -> Entropy.hGetEntropy h n)
