{-# LANGUAGE FlexibleInstances #-}
module Databrary.Resource.Entropy
  ( Entropy
  , initEntropy
  , withEntropy
  ) where
 
import Control.Applicative ((<$>))
import qualified System.Entropy as Entropy

newtype Entropy = Entropy Entropy.CryptHandle

initEntropy :: IO Entropy
initEntropy = Entropy <$> Entropy.openHandle

withEntropy :: Entropy -> (Entropy.CryptHandle -> IO a) -> IO a
withEntropy (Entropy h) f = f h
