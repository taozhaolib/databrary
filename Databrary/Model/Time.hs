{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Time
  (
  ) where

import Database.PostgreSQL.Typed.Types (PGType)
import Database.PostgreSQL.Typed.Range (PGRangeType)

instance PGType "segment"
instance PGRangeType "segment" "interval"
