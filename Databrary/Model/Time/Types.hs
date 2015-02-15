module Databrary.Model.Time.Types
  ( Date
  , Timestamp
  , Offset
  , Segment
  ) where

import qualified Data.Time

import qualified Databrary.JSON as JSON
import Database.PostgreSQL.Typed.Range (Range)

type Date = Data.Time.Day
type Timestamp = Data.Time.UTCTime
type Offset = Data.Time.DiffTime
type Segment = Range Offset

instance JSON.ToJSON Offset where
  toJSON off = JSON.Number (realToFrac off * 1000)
