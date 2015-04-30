{-# LANGUAGE TypeOperators, TupleSections #-}
module Databrary.Iso
  ( Invariant(..)
  , (<->)(..)
  , apply
  , invert
  , (<$>)
  
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

import qualified Control.Category as Cat
import Data.Maybe (fromMaybe)
import qualified Data.Tuple as Tup

import Databrary.Ops ((?!>))

-- see also invariant package
class Invariant f where
  invMap :: (a -> b) -> (b -> a) -> f a -> f b

infix 2 <->, :<->:
data (<->) a b = (:<->:)
  { isoF :: a -> b
  , isoG :: b -> a
  }

apply :: a <-> b -> a -> b
apply = isoF

instance Cat.Category (<->) where
  id = id :<->: id
  (f1 :<->: g1) . (f2 :<->: g2) = f1 . f2 :<->: g2 . g1

invert :: a <-> b -> b <-> a
invert (f :<->: g) = g :<->: f

infixl 4 <$>
(<$>) :: Invariant f => (<->) a b -> f a -> f b
(<$>) (f :<->: g) = invMap f g

swap :: (<->) (a, b) (b, a)
swap = Tup.swap :<->: Tup.swap

switch :: Either a b <-> Either b a
switch = sw :<->: sw where
  sw (Left a) = Right a
  sw (Right a) = Left a

constant :: a -> () <-> a
constant a = const a :<->: const ()

first :: (a, ()) <-> a
first = fst :<->: (, ())

second :: ((), a) <-> a
second = snd :<->: ((), )

left :: Either a () <-> Maybe a
left = either Just (const Nothing) :<->: maybe (Right ()) Left

right :: Either () a <-> Maybe a
right = either (const Nothing) Just :<->: maybe (Left ()) Right

cons :: (a, [a]) <-> [a]
cons = uncurry (:) :<->: uncons where
  uncons (a:l) = (a, l)
  uncons [] = error "uncons: empty list"

curried :: ((a, b) -> c) <-> (a -> b -> c)
curried = curry :<->: uncurry

defaultEq :: Eq a => a -> Maybe a <-> a
defaultEq d = fromMaybe d :<->: \a -> a == d ?!> d
