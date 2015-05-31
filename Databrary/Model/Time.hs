{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Time
  ( Date
  , Timestamp
  ) where

import Data.Fixed (Fixed(..))
import Data.Time (Day(..), UTCTime(..), DiffTime)
import Language.Haskell.TH.Lift (deriveLiftMany)

type Date = Day
type Timestamp = UTCTime

deriveLiftMany [''Fixed, ''DiffTime, ''Day, ''UTCTime]
