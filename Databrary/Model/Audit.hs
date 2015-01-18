module Databrary.Model.Audit
  ( module Databrary.Model.Types.Audit
  , Audit(..)
  ) where

import Databrary.Model.Inet (Inet)
import Databrary.Model.Time (Timestamp)
import Databrary.Model.Id
import Databrary.Model.Types.Audit
import Databrary.Model.SQL.Audit
import Databrary.Model.Party

data AuditIdentity = AuditIdentity
  { auditWho :: !(Id Party)
  , auditIp :: !Inet
  }

data Audit = Audit
  { auditWhen :: !Timestamp
  , auditIdentity :: !AuditIdentity
  , auditAction :: !AuditAction
  }

class AuditM m where
  getAuditIdentity :: m AuditIdentity

instance (HasRequest m, HasIdentity m) => AuditM m
