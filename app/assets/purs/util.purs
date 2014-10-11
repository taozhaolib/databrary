module Util (UnsafeEff(), unsafeEff, returnEff, mapRange) where

import Control.Monad.Eff (Eff())

foreign import data UnsafeEff :: * -> *
foreign import unsafeEff
  "function unsafeEff(eff) {\
    \return eff();\
  \}" :: forall e a . Eff e a -> UnsafeEff a

returnEff :: forall e a . a -> Eff e a
returnEff = return

foreign import mapRange
  "function mapRange(f) {\
    \return function (l) {\
      \var r = [];\
      \for (var i = 0; i < l; i++)\
        \r.push(f(i));\
      \return r;\
    \};\
  \}" :: forall a . (Number -> a) -> Number -> [a]
