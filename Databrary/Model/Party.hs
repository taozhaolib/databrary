{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, GeneralizedNewtypeDeriving, ConstraintKinds #-}
module Databrary.Model.Party 
  ( module Databrary.Model.Types.Party
  , nobodyParty
  , rootParty
  , changeParty
  , lookupParty
  , lookupAccount
  , lookupAuthParty
  ) where

import Control.Monad.Has (Has(..), pull)
import Databrary.DB
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

changeParty :: AuditM m => Party -> m ()
changeParty p = dbExecute1 =<< $(changeQuery 'p)

lookupParty :: DBM m => Id Party -> m (Maybe Party)
lookupParty (Id (-1)) = return $ Just nobodyParty
lookupParty (Id 0) = return $ Just rootParty
lookupParty i = dbQuery1 $(selectQuery partySelector "WHERE party.id = ${i}")

lookupAccount :: DBM m => Id Party -> m (Maybe Account)
lookupAccount (Id i) | i <= 0 = return Nothing
lookupAccount i = dbQuery1 $(selectQuery accountSelector "WHERE account.id = ${i}")

newtype AuthParty = AuthParty (Auth Authorization)

instance Has Identity AuthParty where
  had (AuthParty a) = authIdentity a
instance Has Party AuthParty where
  had (AuthParty a) = authorizeParent (authObject a)

lookupAuthParty :: (DBM m, IdentityM m) => Id Party -> m (Maybe AuthParty)
lookupAuthParty (Id (-1)) = do
  u <- pull
  return $ Just $ AuthParty $ Auth (identityAuthorization u)
    { authorizeParent = nobodyParty
    , authorizeAccess = (authorizeAccess (identityAuthorization u))
      { accessMember = PermissionNONE }
    } u
lookupAuthParty (Id 0) = do
  u <- pull
  return $ Just $ AuthParty $ Auth (identityAuthorization u) u
lookupAuthParty i = do
  u <- pull
  let up = authorizeChild $ identityAuthorization u
  a <- dbQuery1 $(selectQuery' (partyAuthorizationSelector 'up) "WHERE party.id = ${i}")
  return $ fmap (AuthParty . (`Auth` u)) a
