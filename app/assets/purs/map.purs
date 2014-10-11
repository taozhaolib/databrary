module Map (
    Map(..)
  , lookup
  , unsafeLookup
  , invertArray
  ) where

import Data.Maybe (Maybe(..))

foreign import data Map :: * -> *

foreign import lookupFn
  "function lookupFn(d) {\
    \return function(f) {\
      \return function(m) {\
        \return function(k) {\
          \return k in m ? f(m[k]) : d;\
        \};\
      \};\
    \};\
  \}" :: forall a b . b -> (a -> b) -> Map a -> String -> b

lookup :: forall a . Map a -> String -> Maybe a
lookup = lookupFn Nothing Just

foreign import unsafeLookup 
  "function unsafeLookup(m) {\
    \return function(k) {\
      \return m[k];\
    \};\
  \}" :: forall a . Map a -> String -> a

foreign import map
  "function map(f) {\
    \return function (m) {\
      \var r = {};\
      \for (var k in m)\
        \r[k] = f(m[k]);\
      \return r;\
    \};\
  \}" :: forall a b . (a -> b) -> Map a -> Map b

instance functorMap :: Functor Map where
  (<$>) = map

foreign import invertArray 
  "function invertArray(a) {\
    \var r = {};\
    \for (var i = 0; i < a.length; i ++) \
      \r[a[i]] = i;\
    \return r;\
  \}" :: [String] -> Map Number

