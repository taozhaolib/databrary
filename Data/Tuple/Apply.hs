module Data.Tuple.Apply where

import Data.Tuple.Cons

applyT :: ConsT h t l => (h -> a) -> l -> (a, t)
applyT f = first f . unconsT

applyT2 :: (ConsT h t l, ConsT th tt t) => (h -> th -> a) -> l -> (a, tt)
applyT2 f = curry applyT . applyT f
