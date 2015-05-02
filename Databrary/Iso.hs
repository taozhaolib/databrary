{-# LANGUAGE TypeOperators, QuasiQuotes #-}
module Databrary.Iso
  ( module Databrary.Iso.Prim
  
  , not
  , swap
  , constant
  , first
  , second
  , isJust
  , isNothing
  , switch
  , isLeft
  , isRight
  , left
  , right
  , cons
  , curried
  , defaultEq
  ) where

import Prelude hiding (not)

import qualified Control.Category as Cat
import Data.Maybe (fromMaybe)

import Databrary.Ops ((?!>))
import Databrary.Iso.Prim
import Databrary.Iso.TH

not :: Bool <-> Bool
not =
  [iso|
    False <-> True
    True <-> False
  |]

swap :: (a, b) <-> (b, a)
swap = [iso|(a, b) <-> (b, a)|]

constant :: a -> () <-> a
constant a = const a :<->: const ()

first :: (a, ()) <-> a
first = [iso|(a, ()) <-> a|]

second :: ((), a) <-> a
second = [iso|((), a) <-> a|]

isJust :: Maybe () <-> Bool
isJust =
  [iso|
    Just () <-> True
    Nothing <-> False
  |]

isNothing :: Maybe () <-> Bool
isNothing = not Cat.. isJust

switch :: Either a b <-> Either b a
switch =
  [iso|
    Left a <-> Right a
    Right a <-> Left a
  |]

isLeft :: Either () () <-> Bool
isLeft =
  [iso|
    Left () <-> True
    Right () <-> False
  |]

isRight :: Either () () <-> Bool
isRight = not Cat.. isLeft

left :: Either a () <-> Maybe a
left =
  [iso|
    Left a <-> Just a
    Right () <-> Nothing
  |]

right :: Either () a <-> Maybe a
right =
  [iso|
    Left () <-> Nothing
    Right a <-> Just a
  |]

cons :: (a, [a]) <-> [a]
cons = [iso|(a, l) <-> a:l|]

curried :: ((a, b) -> c) <-> (a -> b -> c)
curried = curry :<->: uncurry

defaultEq :: Eq a => a -> Maybe a <-> a
defaultEq d = fromMaybe d :<->: \a -> a == d ?!> d
