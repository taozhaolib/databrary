{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, GeneralizedNewtypeDeriving #-}
module Databrary.Model.Party 
  ( module Databrary.Model.Party.Types
  , PartyAuth
  , AuthParty
  , nobodyParty
  , rootParty
  , partyJSON
  , authPartyJSON

  , changeParty
  , addParty
  , lookupParty
  , lookupAccount
  , lookupAuthParty
  , selfAuthParty
  , lookupPartyAuthByEmail
  , auditAccountLogin
  , recentAccountLogins
  , PartyFilter(..)
  , findParties
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Lens ((^.), set)
import Control.Monad (guard)
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (catMaybes, isNothing, fromMaybe)
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)
import Database.PostgreSQL.Typed.Query (unsafeModifyQuery)
import Database.PostgreSQL.Typed.Dynamic (pgSafeLiteral)
import Database.PostgreSQL.Typed.Types (pgQuote)

import Control.Has (Has(..), peek, peeks, poke)
import Databrary.DB
import qualified Databrary.JSON as JSON
import Databrary.Action.Request
import Databrary.Model.Id
import Databrary.Model.SQL
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Identity.Types
import Databrary.Model.Authorize.Types
import Databrary.Model.Authorize.SQL
import Databrary.Model.Volume.Types
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Party.Boot

useTPG

nobodyParty, rootParty :: Party
nobodyParty = $(loadParty (Id (-1)))
rootParty = $(loadParty (Id 0))

emailPermission :: Permission
emailPermission = PermissionSHARED

showEmail :: Identity -> Bool
showEmail i = i ^. accessSite >= emailPermission

partyEmail :: Party -> Identity -> Maybe T.Text
partyEmail p i =
  guard (showEmail i) >> accountEmail <$> partyAccount p

authPartyPermission :: AuthParty -> Identity -> Permission
authPartyPermission a i =
  see a `max` ((i ^. accessSite) `min` PermissionREAD)

_authPartyEmail :: AuthParty -> Identity -> Maybe T.Text
_authPartyEmail p =
  poke ((see p :: Access) <>) $ partyEmail (see p)

partyJSON :: Party -> Identity -> JSON.Object
partyJSON p@Party{..} i = JSON.record partyId $ catMaybes
  [ Just $ "name" JSON..= partyName
  , ("affiliation" JSON..=) <$> partyAffiliation
  , ("url" JSON..=) <$> partyURL
  , "institution" JSON..= True <$ guard (isNothing partyAccount)
  , ("email" JSON..=) <$> partyEmail p i
  ]

authPartyJSON :: AuthParty -> Identity -> JSON.Object
authPartyJSON a i = partyJSON p i
  JSON..+ ("permission" JSON..= l)
  JSON..+? (guard (l >= emailPermission) >> ("email" JSON..=) . accountEmail <$> partyAccount p)
  where
  p = see a
  l = authPartyPermission a i

changeParty :: AuditM c m => Party -> m ()
changeParty p = dbExecute1 =<< $(updateParty 'p)

addParty :: AuditM c m => Party -> m AuthParty
addParty bp = do
  p <- dbQuery1' =<< $(insertParty 'bp)
  i <- peek
  return $ AuthParty (Authorization mempty i p)

lookupParty :: DBM m => Id Party -> m (Maybe Party)
lookupParty (Id (-1)) = return $ Just nobodyParty
lookupParty (Id 0) = return $ Just rootParty
lookupParty i = dbQuery1 $(selectQuery selectParty "$WHERE party.id = ${i}")

lookupAccount :: DBM m => Id Party -> m (Maybe Account)
lookupAccount (Id i) | i <= 0 = return Nothing
lookupAccount i = dbQuery1 $(selectQuery selectAccount "$WHERE account.id = ${i}")

lookupAuthParty :: (DBM m, IdentityM c m) => Id Party -> m (Maybe AuthParty)
lookupAuthParty i@(Id n) = lap n . partyAuthAuthorization . identityAuthorization =<< peek where
  lap (-1) a =
    return $ Just $ AuthParty $ set accessMember PermissionNONE a
      { authorizeParent = nobodyParty }
  lap 0 a =
    return $ Just $ AuthParty a
  lap _ a =
    fmap AuthParty <$> dbQuery1 $(selectQuery (selectParentAuthorization 'up) "$!WHERE party.id = ${i}")
    where up = authorizeChild a

selfAuthParty :: Party -> AuthParty
selfAuthParty p = AuthParty Authorization
  { authorizeAccess = maxBound
  , authorizeChild = p
  , authorizeParent = p
  }

lookupPartyAuthByEmail :: DBM m => T.Text -> m (Maybe PartyAuth)
lookupPartyAuthByEmail e = fmap PartyAuth <$>
  dbQuery1 $(selectQuery (selectChildAuthorization 'rootParty) "!WHERE account.email = ${e}")

auditAccountLogin :: (RequestM c m, DBM m) => Bool -> Party -> T.Text -> m ()
auditAccountLogin success who email = do
  ip <- getRemoteIp
  dbExecute1 [pgSQL|INSERT INTO audit.account (audit_action, audit_user, audit_ip, id, email) VALUES (${if success then AuditActionOpen else AuditActionAttempt}, -1, ${ip}, ${partyId who}, ${email})|]

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

findParties :: (IdentityM c m, DBM m) => PartyFilter -> Int -> Int -> m [Party]
findParties pf limit offset = do
  q <- peeks $ partyFilter pf
  dbQuery $ $(selectQuery selectParty "") `unsafeModifyQuery`
    (++ q ++ [pgSQL|# LIMIT ${fromIntegral limit :: Int64} OFFSET ${fromIntegral offset :: Int64}|])
