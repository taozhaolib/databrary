{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Time
  ( module Databrary.Model.Time.Types
  ) where

import Database.PostgreSQL.Typed.Types (PGType)
import Database.PostgreSQL.Typed.Range (PGRangeType)

import qualified Databrary.JSON as JSON
import Databrary.Model.Time.Types

instance PGType "segment"
instance PGRangeType "segment" "interval"

instance JSON.ToJSON Offset where
  toJSON off = JSON.Number (realToFrac off * 1000)
