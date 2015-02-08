module Databrary.Entropy
  ( Entropy
  , initEntropy
  , EntropyM
  , entropyBytes
  , entropyBytesGenerator
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified System.Entropy as Entropy

import Control.Has (MonadHas, peek)

newtype Entropy = Entropy Entropy.CryptHandle

initEntropy :: IO Entropy
initEntropy = Entropy <$> Entropy.openHandle

type EntropyM c m = (MonadIO m, MonadHas Entropy c m)

entropyBytesGenerator :: MonadHas Entropy c m => Int -> m (IO ByteString)
entropyBytesGenerator n = do
  Entropy e <- peek
  return $ Entropy.hGetEntropy e n

entropyBytes :: EntropyM c m => Int -> m ByteString
entropyBytes n = liftIO =<< entropyBytesGenerator n
