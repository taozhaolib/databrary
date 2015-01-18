{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Time
  ( Date
  , Timestamp
  , Offset
  , Segment
  ) where

import Data.Time (Day, UTCTime, DiffTime)
import Database.PostgreSQL.Typed.Range (Range, PGRangeType)

type Date = Day
type Timestamp = UTCTime
type Offset = DiffTime
type Segment = Range Offset

instance PGRangeType "segment" "interval"
