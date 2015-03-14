{-# LANGUAGE DataKinds, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Time
  ( module Databrary.Model.Time.Types
  , parseOffset
  , lowerBound, upperBound
  , segmentLength
  , parseSegment
  , age
  , yearsAge
  , ageTime
  ) where

import Control.Applicative ((<$>), (<|>))
import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Fixed (Fixed(..), Milli)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time (diffDays, DiffTime, secondsToDiffTime)
import Database.PostgreSQL.Typed.Types (PGType)
import qualified Database.PostgreSQL.Typed.Range as Range

import qualified Databrary.JSON as JSON
import Databrary.Model.Time.Types

instance PGType "segment"
instance Range.PGRangeType "segment" "interval"

readsOffset :: ReadS Offset
readsOffset os = rm os ++ rc os where
  -- parse milliseconds:
  rm i = do
    (m, r) <- reads i
    case r of
      (':':_) -> []
      _ -> return (realToFrac (MkFixed m :: Milli), r)
  -- parse colons:
  pc i = do
    (n, r) <- reads i
    first (n:) <$> case r of
      (':':r') -> pc r'
      _ -> return ([], r)
  -- parse seconds with colons:
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

instance JSON.ToJSON Offset where
  toJSON off = JSON.Number (realToFrac off * 1000)

instance JSON.FromJSON Offset where
  parseJSON (JSON.Number ms) = return $ realToFrac $ ms / 1000
  parseJSON (JSON.String s) = maybe (fail "Invalid offset string") return $ parseOffset $ T.unpack s
  parseJSON (JSON.Bool False) = return 0
  parseJSON _ = fail "Invalid offset"


lowerBound, upperBound :: Range.Range a -> Maybe a
lowerBound = Range.bound . Range.lowerBound
upperBound = Range.bound . Range.upperBound

segmentLength :: Segment -> Maybe Offset
segmentLength s =
  liftM2 (-) (upperBound s) (lowerBound s)


readsSegment :: ReadS Segment
readsSegment ss = (Range.Empty, fromMaybe ss $ stripPrefix "empty" ss) : plb ss where
  plb ('[':s) = pl (Just True) s
  plb ('(':s) = pl (Just False) s
  plb s = pl Nothing s
  pl lb s = uncurry (pm lb) =<< (Nothing, s) : (first Just <$> readsOffset s)
  pm lb l (',':s) = pu lb l s
  pm lb l ('-':s) = pu lb l s
  pm Nothing (Just l) s = return (Range.point l, s)
  pm _ _ _ = fail "pm"
  pu lb l s = uncurry (pub lb l) =<< (Nothing, s) : (first Just <$> readsOffset s)
  pub lb l u (']':s) = pf lb l u (Just True) s
  pub lb l u (')':s) = pf lb l u (Just False) s
  pub lb l u s = pf lb l u Nothing s
  pf lb l u ub s = return $ (Range.range (mb True lb l) (mb False ub u), s)
  -- more liberal than Range.makeBound:
  mb :: Bool -> Maybe Bool -> Maybe Offset -> Range.Bound Offset
  mb d = maybe Range.Unbounded . Range.Bounded . fromMaybe d

parseSegment :: String -> Maybe Segment
parseSegment = rm . readsSegment where
  rm ((x,""):_) = Just x
  rm (_:l) = rm l
  rm [] = Nothing

instance JSON.ToJSON Segment where
  toJSON s
    | Range.isEmpty s = JSON.Null
    | Just o <- Range.getPoint s = JSON.toJSON o
    | otherwise = JSON.toJSON $ map Range.bound [Range.lowerBound s, Range.upperBound s]

instance JSON.FromJSON Segment where
  parseJSON (JSON.String s) = maybe (fail "Invalid segment string") return $ parseSegment $ T.unpack s
  parseJSON j = do
    a <- JSON.parseJSON j <|> return <$> JSON.parseJSON j
    case a of
      [] -> return Range.empty
      [p] -> return $ maybe Range.empty Range.point p
      [l, u] -> return $ Range.normal l u
      _ -> fail "Segment array too long"

age :: Date -> Date -> Age
age b d = Age $ fromInteger $ diffDays d b

yearsAge :: Real a => a -> Age
yearsAge y = Age $ ceiling $ (365.24219 :: Double) * realToFrac y

ageTime :: Age -> DiffTime
ageTime (Age n) = secondsToDiffTime $ 86400 * fromIntegral n
