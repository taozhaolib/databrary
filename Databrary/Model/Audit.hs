{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Audit
  ( module Databrary.Model.Audit.Types
  , getRemoteIp
  , getAuditIdentity
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Typed.Inet (PGInet(..), sockAddrPGInet)
import Network.Wai (remoteHost)

import Control.Has (peeks)
import Databrary.Action.Request
import Databrary.Identity.Types
import Databrary.Model.Audit.Types

getRemoteIp :: RequestM c m => m PGInet
getRemoteIp = peeks (fromMaybe (PGInet 0 32) . sockAddrPGInet . remoteHost)

getAuditIdentity :: (RequestM c m, IdentityM c m) => m AuditIdentity
getAuditIdentity = AuditIdentity <$> peeks identityId <*> getRemoteIp
