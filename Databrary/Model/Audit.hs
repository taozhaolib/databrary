{-# LANGUAGE TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Audit
  ( module Databrary.Model.Audit.Types
  , MonadAudit
  , getRemoteIp
  , getAuditIdentity
  , Analytic(..)
  , auditAnalytic
  ) where

import Control.Applicative ((<$>), (<*>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Inet (PGInet(..), sockAddrPGInet)
import Network.Wai (remoteHost)

import Control.Has (peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Web.Request
import Databrary.Model.Identity.Types
import Databrary.Model.Audit.Types

useTPG

type MonadAudit c m = (MonadHasRequest c m, MonadHasIdentity c m, DBM m)

getRemoteIp :: MonadHasRequest c m => m PGInet
getRemoteIp = peeks (fromMaybe (PGInet 0 32) . sockAddrPGInet . remoteHost)

getAuditIdentity :: (MonadHasRequest c m, MonadHasIdentity c m) => m AuditIdentity
getAuditIdentity = AuditIdentity <$> peek <*> getRemoteIp

data Analytic = Analytic
  { analyticAction :: AuditAction
  , analyticRoute :: T.Text
  , analyticData :: JSON.Value
  }

auditAnalytic :: (MonadAudit c m) => Analytic -> m ()
auditAnalytic Analytic{..} = do
  ai <- getAuditIdentity
  dbExecute1 [pgSQL|INSERT INTO audit.analytic (audit_action, audit_user, audit_ip, route, data) VALUES
    (${analyticAction}, ${auditWho ai}, ${auditIp ai}, ${analyticRoute}, ${analyticData})|]
