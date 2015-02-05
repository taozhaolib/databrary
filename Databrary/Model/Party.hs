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
  , createParty
  , lookupParty
  , lookupAccount
  , lookupAuthParty
  , selfAuthParty
  , lookupPartyAuthByEmail
  , auditAccountLogin
  , recentAccountLogins
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Monad (guard)
import Data.Int (Int64)
import Data.Maybe (catMaybes, isNothing, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed (pgSQL)

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
import Databrary.Model.Party.Types
import Databrary.Model.Party.SQL
import Databrary.Model.Party.Boot

useTPG

nobodyParty, rootParty :: Party
nobodyParty = $(loadParty (Id (-1)))
rootParty = $(loadParty (Id 0))

partyEmail :: Party -> Identity -> Maybe T.Text
partyEmail p = do
  s <- peeks _accessSite
  r <- checkPermission (s >= PermissionSHARED)
  return $ do
    guard r
    accountEmail <$> partyAccount p

authPartyPermission :: AuthParty -> Identity -> Permission
authPartyPermission a i =
  see a `max` (peeks _accessSite i `min` PermissionREAD)

_authPartyEmail :: AuthParty -> Identity -> Maybe T.Text
_authPartyEmail p =
  poke ((see p :: Access) <>) $ partyEmail (see p)

partyJSON :: Party -> Identity -> JSON.Object
partyJSON p i = JSON.object $ catMaybes
  [ Just $ "name" JSON..= partyName p
  , ("affiliation" JSON..=) <$> partyAffiliation p
  , ("url" JSON..=) <$> partyURL p
  , "institution" JSON..= True <$ guard (isNothing a)
  , ("email" JSON..=) <$> partyEmail p i
  ] where a = partyAccount p

authPartyJSON :: AuthParty -> Identity -> JSON.Object
authPartyJSON a i = partyJSON p i
  JSON..+ ("permission" JSON..= l)
  JSON..+? (guard (l >= PermissionSHARED) >> ("email" JSON..=) . accountEmail <$> partyAccount p)
  where
  p = see a
  l = authPartyPermission a i

changeParty :: AuditM c m => Party -> m ()
changeParty p = dbExecute1 =<< $(changeQuery 'p)

createParty :: AuditM c m => Party -> m Party
createParty p = dbQuery1' =<< $(createQuery 'p)

lookupParty :: DBM m => Id Party -> m (Maybe Party)
lookupParty (Id (-1)) = return $ Just nobodyParty
lookupParty (Id 0) = return $ Just rootParty
lookupParty i = dbQuery1 $(selectQuery partySelector "$WHERE party.id = ${i}")

lookupAccount :: DBM m => Id Party -> m (Maybe Account)
lookupAccount (Id i) | i <= 0 = return Nothing
lookupAccount i = dbQuery1 $(selectQuery accountSelector "$WHERE account.id = ${i}")

lookupAuthParty :: (DBM m, IdentityM c m) => Id Party -> m (Maybe AuthParty)
lookupAuthParty i@(Id n) = lap n . partyAuthAuthorization . identityAuthorization =<< peek where
  lap (-1) a =
    return $ Just $ AuthParty a
      { authorizeParent = nobodyParty
      , authorizeAccess = (authorizeAccess a)
        { _accessMember = PermissionNONE }
      }
  lap 0 a =
    return $ Just $ AuthParty a
  lap _ a =
    fmap AuthParty <$> dbQuery1 $(selectQuery (parentAuthorizationSelector 'up) "$!WHERE party.id = ${i}")
    where up = authorizeChild a

selfAuthParty :: Party -> AuthParty
selfAuthParty p = AuthParty Authorization
  { authorizeAccess = maxBound
  , authorizeChild = p
  , authorizeParent = p
  }

lookupPartyAuthByEmail :: DBM m => T.Text -> m (Maybe PartyAuth)
lookupPartyAuthByEmail e = fmap PartyAuth <$>
  dbQuery1 $(selectQuery (childAuthorizationSelector 'rootParty) "!WHERE account.email = ${e}")

auditAccountLogin :: (RequestM c m, DBM m) => Bool -> Party -> T.Text -> m ()
auditAccountLogin success who email = do
  ip <- getRemoteIp
  dbExecute1 [pgSQL|INSERT INTO audit.account (audit_action, audit_user, audit_ip, id, email) VALUES (${if success then AuditActionOpen else AuditActionAttempt}, -1, ${ip}, ${partyId who}, ${email})|]

recentAccountLogins :: DBM m => Party -> m Int64
recentAccountLogins who = fromMaybe 0 <$>
  dbQuery1 [pgSQL|!SELECT count(*) FROM audit.account WHERE audit_action = 'attempt' AND id = ${partyId who} AND audit_time > CURRENT_TIMESTAMP - interval '1 hour'|]
