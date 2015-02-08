{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Audit
  ( module Databrary.Model.Audit.Types
  , AuditM
  , getRemoteIp
  , getAuditIdentity
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Typed.Inet (PGInet(..), sockAddrPGInet)
import Network.Wai (remoteHost)

import Control.Has (peek, peeks)
import Databrary.DB
import Databrary.Action.Request
import Databrary.Identity.Types
import Databrary.Model.Audit.Types

type AuditM c m = (MonadHasRequest c m, MonadHasIdentity c m, DBM m)

getRemoteIp :: MonadHasRequest c m => m PGInet
getRemoteIp = peeks (fromMaybe (PGInet 0 32) . sockAddrPGInet . remoteHost)

getAuditIdentity :: (MonadHasRequest c m, MonadHasIdentity c m) => m AuditIdentity
getAuditIdentity = AuditIdentity <$> peek <*> getRemoteIp
