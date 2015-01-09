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

data Audit = Audit
  { auditWhen :: !Timestamp
  , auditWho :: Id Party
  , auditIp :: !Inet
  , auditAction :: !AuditAction
  }
