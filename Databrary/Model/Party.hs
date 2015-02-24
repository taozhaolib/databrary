{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Party 
  ( module Databrary.Model.Party.Types
  , nobodyParty
  , rootParty
  , lookupParty
  , lookupAuthParty
  , lookupSiteAuthByEmail
  , changeParty
  , changeAccount
  , addParty
  , addAccount
  , auditAccountLogin
  , recentAccountLogins
  , partyJSON
  , PartyFilter(..)
  , findParties
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isNothing, fromMaybe)
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (unsafeModifyQuery)
import Database.PostgreSQL.Typed.Dynamic (pgSafeLiteral)
import Database.PostgreSQL.Typed.Types (pgQuote)

import Control.Applicative.Ops
import Control.Has (Has(..), peek)
import Databrary.DB
import qualified Databrary.JSON as JSON
import Databrary.Web.Request
import Databrary.Model.Id
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Model.Identity.Types
import Databrary.Model.Volume.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Party.Boot

useTPG

nobodyParty, rootParty :: Party
nobodyParty = $(loadParty (Id (-1)) PermissionREAD)
rootParty = $(loadParty (Id 0) PermissionSHARED)

emailPermission :: Permission
emailPermission = PermissionSHARED

showEmail :: Identity -> Bool
showEmail i = accessSite i >= emailPermission

partyEmail :: Party -> Maybe T.Text
partyEmail p =
  guard (partyPermission p >= emailPermission) >> accountEmail <$> partyAccount p

partyJSON :: Party -> JSON.Object
partyJSON p@Party{..} = JSON.record partyId $ catMaybes
  [ Just $ "name" JSON..= partyName
  , ("affiliation" JSON..=) <$> partyAffiliation
  , ("url" JSON..=) <$> partyURL
  , "institution" JSON..= True <? isNothing partyAccount
  , ("email" JSON..=) <$> partyEmail p
  , "permission" JSON..= partyPermission <? (partyPermission > PermissionREAD)
  ]

changeParty :: AuditM c m => Party -> m ()
changeParty p = do
  ident <- getAuditIdentity
  dbExecute1 $(updateParty 'ident 'p)

changeAccount :: AuditM c m => Account -> m ()
changeAccount a = do
  ident <- getAuditIdentity
  dbExecute1 $(updateAccount 'ident 'a)

addParty :: AuditM c m => Party -> m Party
addParty bp = do
  ident <- getAuditIdentity
  dbQuery1' $ fmap ($ PermissionREAD) $(insertParty 'ident 'bp)

addAccount :: AuditM c m => Account -> m Account
addAccount ba@Account{ accountParty = bp } = do
  ident <- getAuditIdentity
  p <- dbQuery1' $ fmap ($ PermissionREAD) $(insertParty 'ident 'bp)
  let pa = p{ partyAccount = Just a }
      a = ba{ accountParty = pa }
  dbExecute1 $(insertAccount 'ident 'a)
  return a

lookupFixedParty :: Id Party -> Identity -> Maybe Party
lookupFixedParty (Id (-1)) _ = Just nobodyParty
lookupFixedParty (Id 0) _ = Just rootParty
lookupFixedParty i a = view a <? (i == view a)

lookupParty :: (DBM m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupParty i = do
  ident <- peek
  maybe
    (dbQuery1 $(selectQuery (selectParty 'ident) "$WHERE party.id = ${i}"))
    (return . Just) $ lookupFixedParty i ident

lookupAuthParty :: (DBM m, MonadHasIdentity c m) => Id Party -> m (Maybe Party)
lookupAuthParty i = do
  ident <- peek
  maybe
    (dbQuery1 $(selectQuery (selectAuthParty 'ident) "$WHERE party.id = ${i}"))
    (return . Just) $ lookupFixedParty i ident

lookupSiteAuthByEmail :: DBM m => T.Text -> m (Maybe SiteAuth)
lookupSiteAuthByEmail e =
  dbQuery1 $(selectQuery selectSiteAuth "WHERE account.email = ${e}")

auditAccountLogin :: (MonadHasRequest c m, DBM m) => Bool -> Party -> T.Text -> m ()
auditAccountLogin success who email = do
  ip <- getRemoteIp
  dbExecute1 [pgSQL|INSERT INTO audit.account (audit_action, audit_user, audit_ip, id, email) VALUES
    (${if success then AuditActionOpen else AuditActionAttempt}, -1, ${ip}, ${partyId who}, ${email})|]

recentAccountLogins :: DBM m => Party -> m Int64
recentAccountLogins who = fromMaybe 0 <$>
  dbQuery1 [pgSQL|!SELECT count(*) FROM audit.account WHERE audit_action = 'attempt' AND id = ${partyId who} AND audit_time > CURRENT_TIMESTAMP - interval '1 hour'|]

data PartyFilter = PartyFilter
  { partyFilterQuery :: Maybe String
  , partyFilterAccess :: Maybe Permission
  , partyFilterInstitution :: Maybe Bool
  , partyFilterAuthorize :: Maybe Party
  , partyFilterVolume :: Maybe Volume
  }

instance Monoid PartyFilter where
  mempty = PartyFilter Nothing Nothing Nothing Nothing Nothing
  mappend (PartyFilter q1 a1 i1 p1 v1) (PartyFilter q2 a2 i2 p2 v2) =
    PartyFilter (q1 <> q2) (a1 <|> a2) (i1 <|> i2) (p1 <|> p2) (v1 <|> v2)

partyFilter :: PartyFilter -> Identity -> String
partyFilter PartyFilter{..} ident =
  withq partyFilterAccess (const " JOIN authorize_view ON party.id = child AND parent = 0")
  ++ " WHERE id > 0"
  ++ withq partyFilterQuery (\n -> " AND " ++ queryVal ++ " ILIKE " ++ pgQuote (wordPat n))
  ++ withq partyFilterAccess (\a -> " AND site = " ++ pgSafeLiteral a)
  ++ withq partyFilterInstitution (\i -> if i then " AND account.id IS NULL" else " AND account.password IS NOT NULL")
  ++ withq partyFilterAuthorize (\a -> let i = pgSafeLiteral (partyId a) in " AND party.id <> " ++ i ++ " AND id NOT IN (SELECT child FROM authorize WHERE parent = " ++ i ++ " UNION SELECT parent FROM authorize WHERE child = " ++ i ++ ")")
  ++ withq partyFilterVolume (\v -> " AND id NOT IN (SELECT party FROM volume_access WHERE volume = " ++ pgSafeLiteral (volumeId v) ++ ")")
  where
  withq v f = maybe "" f v
  wordPat = intercalate "%" . ("":) . (++[""]) . words
  queryVal
    | showEmail ident = "(name || COALESCE(' ' || email, ''))"
    | otherwise = "name"

findParties :: (MonadHasIdentity c m, DBM m) => PartyFilter -> Int -> Int -> m [Party]
findParties pf limit offset = do
  ident <- peek
  dbQuery $ unsafeModifyQuery $(selectQuery (selectParty 'ident) "")
    (++ partyFilter pf ident ++ [pgSQL|# LIMIT ${fromIntegral limit :: Int64} OFFSET ${fromIntegral offset :: Int64}|])
