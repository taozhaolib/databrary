{-# LANGUAGE DataKinds, TemplateHaskell, StandaloneDeriving, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Types.Audit
  ( AuditAction(..)
  , AuditIdentity(..)
  , Audit(..)
  , AuditM
  ) where

import Data.Char (toUpper)
import Database.PostgreSQL.Typed.Enum (makePGEnum)
import Database.PostgreSQL.Typed.Inet (PGInet)

import Databrary.Types.Time
import Databrary.DB
import Databrary.Model.Types.Id
import Databrary.Model.Types.Party
import Databrary.Types.Identity
import Databrary.Action.Request

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

type AuditM c m = (RequestM c m, IdentityM c m, DBM m)
