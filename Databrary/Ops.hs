module Databrary.Ops
  ( (<$>)
  , (<$), ($>)
  , (<?), (?>)
  , (<!?), (?!>)
  , (?$), ($?)
  , (>$), ($<)
  , fromMaybeM
  , orElseM
  , flatMapM
  ) where

import Control.Applicative
import qualified Data.Foldable as Fold
import Data.Functor
import Data.Monoid (Endo(..))

infixl 1 <?, <!?
infixr 1 ?>, ?!>

-- |@'($>)' . guard@ 
(?>) :: Alternative f => Bool -> a -> f a
False ?> _ = empty
True ?> a = pure a

-- |@flip '(?>)'@
(<?) :: Alternative f => a -> Bool -> f a
_ <? False = empty
a <? True = pure a

-- |@'(?>)' . not@
(?!>) :: Alternative f => Bool -> a -> f a
True ?!> _ = empty
False ?!> a = pure a

-- |@flip '(?!>)'@
(<!?) :: Alternative f => a -> Bool -> f a
_ <!? True = empty
a <!? False = pure a

{-# SPECIALIZE (?>) :: Bool -> a -> Maybe a #-}
{-# SPECIALIZE (<?) :: a -> Bool -> Maybe a #-}
{-# SPECIALIZE (?!>) :: Bool -> a -> Maybe a #-}
{-# SPECIALIZE (<!?) :: a -> Bool -> Maybe a #-}


infixr 0 ?$
infixl 0 $?

-- |Fold over endomorphic composition.
(?$) :: (Functor f, Fold.Foldable f) => f (a -> a) -> a -> a
(?$) = appEndo . Fold.fold . fmap Endo

-- |@flip '(?$)'@
($?) :: (Functor f, Fold.Foldable f) => a -> f (a -> a) -> a
($?) = flip (?$)

{-# SPECIALIZE (?$) :: Maybe (a -> a) -> a -> a #-}
{-# SPECIALIZE ($?) :: a -> Maybe (a -> a) -> a #-}

infix 4 >$, $<

-- |@(=<<) ($>)@
(>$) :: Functor f => (a -> f ()) -> a -> f a
f >$ a = f a $> a

-- |@flip '(>$)'@
($<) :: Functor f => a -> (a -> f ()) -> f a
a $< f = a <$ f a

fromMaybeM :: Monad m => m a -> Maybe a -> m a
fromMaybeM _ (Just a) = return a
fromMaybeM m Nothing = m

infixl 3 `orElseM`

orElseM :: Monad m => Maybe a -> m (Maybe a) -> m (Maybe a)
orElseM Nothing m = m
orElseM m _ = return m

flatMapM :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
flatMapM = maybe (return Nothing)
