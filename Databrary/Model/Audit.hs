{-# LANGUAGE ConstraintKinds #-}
module Databrary.Model.Audit
  ( module Databrary.Model.Types.Audit
  , getAuditIdentity
  ) where

import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Typed.Inet (PGInet(..), sockAddrPGInet)
import Network.Wai (remoteHost)

import Control.Has (peek)
import Databrary.Model.Types.Party
import Databrary.Model.Types.Authorize
import Databrary.Types.Identity
import Databrary.Model.Types.Audit

getAuditIdentity :: AuditM c m => m AuditIdentity
getAuditIdentity = do
  req <- peek
  ident <- peek
  return $ AuditIdentity
    { auditWho = partyId $ authorizeChild $ identityAuthorization ident
    , auditIp = fromMaybe (PGInet 0 32) $ sockAddrPGInet $ remoteHost req
    }
