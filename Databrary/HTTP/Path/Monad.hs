module Databrary.HTTP.Path.Monad
  ( PathM(..)
  , pathHead
  , fullPathM
  ) where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Arrow (first)
import Control.Monad (MonadPlus(..), ap, (<=<), join)
import Control.Monad.State.Class (MonadState(..))
import qualified Data.Text as T

import Databrary.HTTP.Route.Types

newtype PathM a = PathM { runPathM :: Path -> Maybe (a, Path) }

instance Functor PathM where
  fmap f (PathM a) = PathM $ fmap (first f) . a

instance Applicative PathM where
  pure = return
  (<*>) = ap

instance Monad PathM where
  return a = PathM $ \p -> Just (a, p)
  PathM x >>= f = PathM $
    uncurry (runPathM . f) <=< x
  fail _ = mzero

instance MonadPlus PathM where
  mzero = PathM $ \_ -> Nothing
  PathM a `mplus` PathM b = PathM $ \t ->
    a t `mplus` b t

instance Alternative PathM where
  empty = mzero
  (<|>) = mplus

instance MonadState Path PathM where
  get = PathM $ Just . join (,)
  put t = PathM $ \_ -> Just ((), t)
  state f = PathM $ Just . f

uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (a:l) = Just (a, l)

pathHead :: PathM T.Text
pathHead = PathM uncons

fullPathM :: PathM a -> Path -> Maybe a
fullPathM (PathM f) p = do
  (a, []) <- f p
  return a
