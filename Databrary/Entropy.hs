{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Databrary.Entropy
  ( MonadEntropy(..)
  , entropyBytes
  ) where

import Data.ByteString (ByteString)
import qualified System.Entropy as Entropy

class MonadEntropy m where
  liftEntropy :: (Entropy.CryptHandle -> IO a) -> m a

instance (MonadIO m, HasResource m) => MonadEntropy m where
  liftEntropy f = do
    e <- getResource resourceEntropy
    liftIO $ withEntropy e f

entropyBytes :: MonadEntropy m => Int -> m ByteString
entropyBytes n =
  liftEntropy (\h -> Entropy.hGetEntropy h n)
