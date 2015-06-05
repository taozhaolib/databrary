module Databrary.Service.Entropy
  ( Entropy
  , initEntropy
  , finiEntropy
  , entropyBytes
  ) where

import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import qualified System.Entropy as Entropy

newtype Entropy = Entropy Entropy.CryptHandle

initEntropy :: IO Entropy
initEntropy = Entropy <$> Entropy.openHandle

finiEntropy :: Entropy -> IO ()
finiEntropy (Entropy h) = Entropy.closeHandle h

entropyBytes :: Int -> Entropy -> IO ByteString
entropyBytes n (Entropy e) = Entropy.hGetEntropy e n
