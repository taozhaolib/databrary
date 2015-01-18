{-# LANGUAGE DataKinds, TemplateHaskell, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Types.Audit
  ( AuditAction(..)
  ) where

import Data.Char (toUpper)
import Database.PostgreSQL.Typed.Enum (makePGEnum)

import Databrary.DB (useTPG)

useTPG

makePGEnum "audit_action" "AuditAction" (\(h:r) -> "AuditAction" ++ toUpper h:r)
