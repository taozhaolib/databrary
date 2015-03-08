{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Time
  ( module Databrary.Model.Time.Types
  , parseOffset
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Fixed (Fixed(..), Milli)
import Database.PostgreSQL.Typed.Types (PGType)
import Database.PostgreSQL.Typed.Range (PGRangeType)

import qualified Databrary.JSON as JSON
import Databrary.Model.Time.Types

instance PGType "segment"
instance PGRangeType "segment" "interval"

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
