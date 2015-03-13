{-# LANGUAGE DataKinds, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Time
  ( module Databrary.Model.Time.Types
  , parseOffset
  , lowerBound, upperBound
  , segmentLength
  , age
  , yearsAge
  , ageTime
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Fixed (Fixed(..), Milli)
import Data.Time (diffDays, DiffTime, secondsToDiffTime)
import Database.PostgreSQL.Typed.Types (PGType)
import qualified Database.PostgreSQL.Typed.Range as Range

import qualified Databrary.JSON as JSON
import Databrary.Model.Time.Types

instance PGType "segment"
instance Range.PGRangeType "segment" "interval"

instance JSON.ToJSON Offset where
  toJSON off = JSON.Number (realToFrac off * 1000)

readsOffset :: ReadS Offset
readsOffset os = rm os ++ rc os where
  rm i = do
    (m, r) <- reads i
    case r of
      (':':_) -> []
      _ -> return (realToFrac (MkFixed m :: Milli), r)
  pc i = do
    (n, r) <- reads i
    first (n:) <$> case r of
      (':':r') -> pc r'
      _ -> return ([], r)
  rc si = do
    let (sign,i) = case si of
          '-':s -> (negate, s)
          '+':s -> (id, s)
          s -> (id, s)
    (c, r) <- pc i
    flip (,) r . realToFrac . sign <$> case c of
      [s] -> return (s :: Milli)
      [m, s] -> return (60*m + s)
      [h, m, s] -> return (60*(60*h + m) + s)
      [d, h, m, s] -> return (60*(60*(24*d + h) + m) + s)
      _ -> []

parseOffset :: String -> Maybe Offset
parseOffset = rm . readsOffset where
  rm ((x,""):_) = Just x
  rm (_:l) = rm l
  rm [] = Nothing

lowerBound, upperBound :: Range.Range a -> Maybe a
lowerBound = Range.bound . Range.lowerBound
upperBound = Range.bound . Range.upperBound

segmentLength :: Segment -> Maybe Offset
segmentLength s =
  liftM2 (-) (upperBound s) (lowerBound s)

instance JSON.ToJSON Segment where
  toJSON s
    | Range.isEmpty s = JSON.Null
    | Just o <- Range.getPoint s = JSON.toJSON o
    | otherwise = JSON.toJSON $ map Range.bound [Range.lowerBound s, Range.upperBound s]

age :: Date -> Date -> Age
age b d = Age $ fromInteger $ diffDays d b

yearsAge :: Real a => a -> Age
yearsAge y = Age $ ceiling $ (365.24219 :: Double) * realToFrac y

ageTime :: Age -> DiffTime
ageTime (Age n) = secondsToDiffTime $ 86400 * fromIntegral n
