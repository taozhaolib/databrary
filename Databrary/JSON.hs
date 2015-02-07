module Databrary.JSON
  ( module Data.Aeson.Types
  , object
  , (.+)
  , (.+?)
  , (.++)
  ) where

import Data.Aeson.Types hiding (object)
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')

object :: [Pair] -> Object
object = HM.fromList

infixl 4 .+, .+?, .++
(.+) :: Object -> Pair -> Object
o .+ (k, v) = HM.insert k v o

(.+?) :: Object -> Maybe Pair -> Object
o .+? Nothing = o
o .+? Just p = o .+ p

(.++) :: Object -> [Pair] -> Object
(.++) = foldl' (.+)
