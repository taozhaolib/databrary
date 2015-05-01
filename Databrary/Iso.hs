{-# LANGUAGE TypeOperators, QuasiQuotes #-}
module Databrary.Iso
  ( module Databrary.Iso.Prim
  
  , constant
  , swap
  , switch
  , first
  , second
  , left
  , right
  , cons
  , curried
  , defaultEq
  ) where

import Data.Maybe (fromMaybe)

import Databrary.Ops ((?!>))
import Databrary.Iso.Prim
import Databrary.Iso.TH

swap :: (a, b) <-> (b, a)
swap = [iso|(a, b) <-> (b, a)|]

switch :: Either a b <-> Either b a
switch =
  [iso|
    Left a <-> Right a
    Right a <-> Left a
  |]

constant :: a -> () <-> a
constant a = const a :<->: const ()

first :: (a, ()) <-> a
first = [iso|(a, ()) <-> a|]

second :: ((), a) <-> a
second = [iso|((), a) <-> a|]

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
