module Databrary.JSON
  ( module Data.Aeson.Types
  , object
  , (.+)
  , (.+?)
  , (.++)
  , parseEnum
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

parseEnum :: forall a . (Bounded a, Enum a) => String -> Value -> Parser a
parseEnum s = withScientific s (p . floor) where
  p i
    | i < fe minBound || i > fe maxBound = fail $ s ++ " out of range"
    | otherwise = return $ toEnum i
  fe :: a -> Int
  fe = fromEnum
