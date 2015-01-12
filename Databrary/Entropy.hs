module Databrary.Entropy
  ( entropyBytes
  ) where

import qualified System.Entropy as Entropy

import Databrary.Resource.Entropy

entropyBytes :: (MonadIO m, MonadReader b m, HasEntropy b) => Int -> m BS.ByteString
entropyBytes n =
  liftEntropy (\h -> Entropy.hGetEntropy h n)
