module Databrary.Resource.Entropy
  ( Entropy
  , HasEntropy(..)
  , initEntropy
  , liftEntropy
  ) where
 
import Control.Applicative ((<$>))
import Control.Lens (Lens', view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import qualified System.Entropy as Entropy

newtype Entropy = Entropy Entropy.CryptHandle

initEntropy :: IO Entropy
initEntropy = Entropy <$> Entropy.openHandle

class HasEntropy b where
  entropyLens :: Lens' b Entropy

liftEntropy :: (MonadIO m, MonadReader b m, HasEntropy b) => (Entropy.CryptHandle -> IO a) -> m a
liftEntropy f = do
  Entropy h <- view entropyLens
  liftIO $ f h
