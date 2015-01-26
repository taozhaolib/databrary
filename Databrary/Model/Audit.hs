{-# LANGUAGE TemplateHaskell, QuasiQuotes, ConstraintKinds #-}
module Databrary.Model.Audit
  ( module Databrary.Model.Types.Audit
  , getRemoteIp
  , getAuditIdentity
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Typed.Inet (PGInet(..), sockAddrPGInet)
import Network.Wai (remoteHost)

import Control.Has (peeks)
import Databrary.Action.Types
import Databrary.Types.Identity
import Databrary.Model.Types.Audit

getRemoteIp :: RequestM c m => m PGInet
getRemoteIp = peeks (fromMaybe (PGInet 0 32) . sockAddrPGInet . remoteHost)

getAuditIdentity :: (RequestM c m, IdentityM c m) => m AuditIdentity
getAuditIdentity = AuditIdentity <$> peeks identityId <*> getRemoteIp
