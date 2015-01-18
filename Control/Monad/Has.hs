{-# LANGUAGE DefaultSignatures #-}
module Control.Monad.Has
  ( Has(..)
  , HasM
  , pull
  , pulls
  ) where

import Control.Monad.Reader (MonadReader, ReaderT, asks)
import qualified Control.Monad.RWS.Strict as RWS
import qualified Control.Monad.RWS.Lazy as RWSL
import Data.Monoid (Monoid)

class Has a c where
  had :: c -> a 

instance Has a a where
  had = id

class Monad m => HasM a m where
  pull :: m a
  default pull :: (MonadReader c m, Has a c) => m a
  pull = asks had
  pulls :: (a -> b) -> m b
  -- pulls f = liftM f pull
  default pulls :: (MonadReader c m, Has a c) => (a -> b) -> m b
  pulls f = asks (f . had)

instance (Monad m, Has a b) => HasM a (ReaderT b m)
instance (Monad m, Monoid w, Has a b) => HasM a (RWS.RWST b w s m)
instance (Monad m, Monoid w, Has a b) => HasM a (RWSL.RWST b w s m)
