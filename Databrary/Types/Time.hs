module Databrary.Types.Time
  ( Date
  , Timestamp
  , Offset
  , Segment
  ) where

import qualified Data.Time

import Database.PostgreSQL.Typed.Range (Range)

type Date = Data.Time.Day
type Timestamp = Data.Time.LocalTime -- FIXME: Data.Time.UTCTime
type Offset = Data.Time.DiffTime
type Segment = Range Offset
