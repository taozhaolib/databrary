{-# LANGUAGE DataKinds, PatternGuards #-}
module Databrary.Model.Segment
  ( Segment
  , lowerBound, upperBound
  , segmentLength
  , readsSegment
  , parseSegment
  , showSegment
  ) where

import Control.Applicative ((<$>), (<|>))
import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Types (PGType)
import qualified Database.PostgreSQL.Typed.Range as Range

import qualified Databrary.JSON as JSON
import Databrary.Model.Offset

type Segment = Range.Range Offset

instance PGType "segment"
instance Range.PGRangeType "segment" "interval"

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

showSegment :: Segment -> String
showSegment Range.Empty = "empty"
showSegment r | Just p <- Range.getPoint r = showOffset p
showSegment r =
  maybe "" ((if Range.lowerClosed r then id else ('(' :)) . showOffset) (lowerBound r)
  ++ ',' : maybe "" showOffset (upperBound r)
  ++ (if Range.upperClosed r then "]" else "")

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
