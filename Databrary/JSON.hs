{-# LANGUAGE OverloadedStrings #-}
module Databrary.JSON
  ( module Data.Aeson.Types
  , object
  , record
  , (.+)
  , (.+?)
  , (.++)
  ) where

import Data.Aeson.Types hiding (object)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')

object :: [Pair] -> Object
object = HM.fromList

record :: ToJSON k => k -> [Pair] -> Object
record k = object . (("id" .= k) :)

infixl 4 .+, .+?, .++
(.+) :: Object -> Pair -> Object
o .+ (k, v) = HM.insert k v o

(.+?) :: Object -> Maybe Pair -> Object
o .+? Nothing = o
o .+? Just p = o .+ p

(.++) :: Object -> [Pair] -> Object
(.++) = foldl' (.+)
