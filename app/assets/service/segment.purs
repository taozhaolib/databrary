module Segment (
    Segments()
  , Segment(..)
  ) where

import Data.Function
import Angular.DI (Dependency)

foreign import data Segments :: *

newtype Segment = Segment { l :: Number, u :: Number }

instance serviceSegment :: Dependency Segments where
  name = "Segment"

foreign import newSegmentFn
  "function newSegment(Segment, l, u) {\
  \ return new Segment(l, u);\
  \}" :: Fn3 Segments Number Number Segment

newSegment :: Segments -> Number -> Number -> Segment
newSegment = runFn3 newSegmentFn

foreign import full
  "function full(segment) {\
  \ return segment.full;\
  \}" :: Segment -> Boolean

foreign import empty
  "function empty(segment) {\
  \ return segment.full;\
  \}" :: Segment -> Boolean

foreign import base
  "function base(segment) {\
  \ return segment.base;\
  \}" :: Segment -> Number

foreign import getFull
  "function getFull(Segment) {\
  \ return Segment.full;\
  \}" :: Segments -> Segment

foreign import getEmpty
  "function getEmpty(Segment) {\
  \ return Segment.empty;\
  \}" :: Segments -> Segment

foreign import format
  "function format(segment) {\
  \ return segment.format();\
  \}" :: Segment -> String

foreign import intersect
  "function intersect(s1) {\
  \ return function (s2) {\
  \   return s1.intersect(s2);\
  \ };\
  \}" :: Segment -> Segment -> Segment

instance showSegment :: Show Segment where
  show = format

foreign import union
  "function union(s1) {\
  \ return function (s2) {\
  \   return s1.union(s2);\
  \ };\
  \}" :: Segment -> Segment -> Segment

foreign import overlaps
  "function overlaps(s1) {\
  \ return function (s2) {\
  \   return s1.overlaps(s2);\
  \ };\
  \}" :: Segment -> Segment -> Boolean

foreign import containsSegment
  "function containsSegment(s1) {\
  \ return function (s2) {\
  \   return s1.contains(s2);\
  \ };\
  \}" :: Segment -> Segment -> Boolean

foreign import containsPoint
  "function containsPoint(s1) {\
  \ return function (p2) {\
  \   return s1.contains(p2);\
  \ };\
  \}" :: Segment -> Number -> Boolean
