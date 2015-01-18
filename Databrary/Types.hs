module Databrary.Types
  ( Timestamp
  ) where

import qualified Data.Time

type Timestamp = Data.Time.LocalTime -- FIXME: Data.Time.UTCTime
