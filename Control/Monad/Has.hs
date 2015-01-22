{-# LANGUAGE ConstraintKinds #-}
module Control.Monad.Has
  ( Has(..)
  , HasM
  , pull
  , pulls
  ) where

import Control.Monad.Reader (MonadReader, asks)

class Has a c where
  had :: c -> a 

instance Has a a where
  had = id

type HasM a c m = (MonadReader c m, Has a c)

pull :: HasM a c m => m a
pull = asks had

pulls :: HasM a c m => (a -> b) -> m b
pulls f = asks (f . had)
