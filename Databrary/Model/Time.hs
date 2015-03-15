module Databrary.Model.Time
  ( Date
  , Timestamp
  ) where

import Data.Time (Day, UTCTime)

type Date = Day
type Timestamp = UTCTime
