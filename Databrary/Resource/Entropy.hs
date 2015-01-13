{-# LANGUAGE FlexibleInstances #-}
module Databrary.Resource.Entropy
  ( Entropy
  , HasEntropy(..)
  , initEntropy
  , withEntropy
  ) where
 
import Control.Applicative ((<$>))
import Control.Monad.Trans.Class (MonadTrans, lift)
import qualified System.Entropy as Entropy

newtype Entropy = Entropy Entropy.CryptHandle

initEntropy :: IO Entropy
initEntropy = Entropy <$> Entropy.openHandle

class HasEntropy m where
  getEntropy :: m Entropy

instance (MonadTrans t, Monad m, HasEntropy m) => HasEntropy (t m) where
  getEntropy = lift getEntropy

withEntropy :: Entropy -> (Entropy.CryptHandle -> IO a) -> IO a
withEntropy (Entropy h) f = f h
