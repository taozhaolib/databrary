module Control.Applicative.Ops
  ( (<$>)
  , (<$), ($>)
  , (<?), (?>)
  , (<!?), (?!>)
  ) where

import Control.Applicative

infixl 4     <?, <!?
infixr 4 $>, ?>, ?!>

-- |@flip (<$)@.
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)

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
