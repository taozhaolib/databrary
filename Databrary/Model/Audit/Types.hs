{-# LANGUAGE DataKinds, TemplateHaskell, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Audit.Types
  ( AuditAction(..)
  , AuditIdentity(..)
  , Audit(..)
  ) where

import Data.Char (toUpper)
import Database.PostgreSQL.Typed.Enum (makePGEnum)
import Database.PostgreSQL.Typed.Inet (PGInet)

import Databrary.Time
import Databrary.DB
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types

useTPG

makePGEnum "audit.action" "AuditAction" (\(h:r) -> "AuditAction" ++ toUpper h:r)

data AuditIdentity = AuditIdentity
  { auditWho :: !(Id Party)
  , auditIp :: !PGInet
  }

data Audit = Audit
  { auditWhen :: !Timestamp
  , auditIdentity :: !AuditIdentity
  , auditAction :: !AuditAction
  }
