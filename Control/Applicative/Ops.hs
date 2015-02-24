module Control.Applicative.Ops
  ( (<$>)
  , (<$), ($>)
  , (<?), (?>)
  , (<!?), (?!>)
  , (?$), ($?)
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
