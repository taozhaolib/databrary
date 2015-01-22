{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, GeneralizedNewtypeDeriving, ConstraintKinds #-}
module Databrary.Model.Party 
  ( module Databrary.Model.Types.Party
  , nobodyParty
  , rootParty
  , changeParty
  , lookupParty
  , lookupAccount
  , lookupAuthParty
  , partyJSON
  , authPartyJSON
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Monad (liftM, guard)
import Control.Monad.Reader.Class (ask)
import Data.Maybe (catMaybes, isNothing)
import Data.Monoid ((<>))
import qualified Data.Text as T

import Control.Monad.Has (Has(..), pull)
import Databrary.DB
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.SQL.Party
import Databrary.Model.SQL.Authorize
import Databrary.Model.SQL (selectQuery, selectQuery')
import Databrary.Model.Permission
import Databrary.Model.Audit
import Databrary.Types.Identity
import Databrary.Model.Types.Authorize
import Databrary.Model.Types.Party

useTPG

newtype AuthParty = AuthParty Authorization

instance Has Party AuthParty where
  had (AuthParty a) = authorizeParent a

instance Has Access AuthParty where
  had (AuthParty a) = had a

nobodyParty :: Party
nobodyParty = Party
  { partyId = Id (-1)
  , partyName = "Everybody"
  , partyAffiliation = Nothing
  , partyURL = Nothing
  , partyAccount = Nothing
  }

rootParty :: Party
rootParty = Party
  { partyId = Id 0
  , partyName = "Databrary"
  , partyAffiliation = Nothing
  , partyURL = Nothing
  , partyAccount = Nothing
  }

partyEmail :: Party -> Identity -> Maybe T.Text
partyEmail p = do
  s <- accessSite <$> ask
  r <- checkPermission (s >= PermissionSHARED)
  return $ do
    guard r
    accountEmail <$> partyAccount p

authPartyPermission :: AuthParty -> Identity -> Permission
authPartyPermission a = do
  s <- accessSite <$> ask
  return $ accessPermission a `max` (s `min` PermissionREAD)

authPartyEmail :: AuthParty -> Identity -> Maybe T.Text
authPartyEmail p i@Identity{ identityAuthorization = a } =
  partyEmail (had p) i{ identityAuthorization = a{ authorizeAccess = had p <> authorizeAccess a } }

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
  p = had a
  l = authPartyPermission a i

changeParty :: AuditM c m => Party -> m ()
changeParty p = dbExecute1 =<< $(changeQuery 'p)

lookupParty :: DBM m => Id Party -> m (Maybe Party)
lookupParty (Id (-1)) = return $ Just nobodyParty
lookupParty (Id 0) = return $ Just rootParty
lookupParty i = dbQuery1 $(selectQuery partySelector "WHERE party.id = ${i}")

lookupAccount :: DBM m => Id Party -> m (Maybe Account)
lookupAccount (Id i) | i <= 0 = return Nothing
lookupAccount i = dbQuery1 $(selectQuery accountSelector "WHERE account.id = ${i}")

lookupAuthParty :: (DBM m, IdentityM c m) => Id Party -> m (Maybe AuthParty)
lookupAuthParty i@(Id n) = lap n . identityAuthorization =<< pull where
  lap (-1) a =
    return $ Just $ AuthParty a
      { authorizeParent = nobodyParty
      , authorizeAccess = (authorizeAccess a)
        { _accessMember = PermissionNONE }
      }
  lap 0 a =
    return $ Just $ AuthParty a
  lap _ a =
    fmap AuthParty `liftM` dbQuery1 $(selectQuery' (partyAuthorizationSelector 'up) "WHERE party.id = ${i}")
    where up = authorizeChild a
