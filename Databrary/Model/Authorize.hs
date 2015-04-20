{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Authorize
  ( module Databrary.Model.Authorize.Types
  , selfAuthorize
  , lookupAuthorizedChildren
  , lookupAuthorizedParents
  , lookupAuthorize
  , lookupAuthorizeParent
  , lookupAuthorization
  , changeAuthorize
  , removeAuthorize
  , authorizeJSON
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Database.PostgreSQL.Typed.Query (PGQuery, unsafeModifyQuery)

import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Model.SQL
import Databrary.Model.Id
import Databrary.Model.Audit
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Model.Identity.Types
import Databrary.Model.Authorize.Types
import Databrary.Model.Authorize.SQL

useTPG

selfAuthorize :: Party -> Authorize
selfAuthorize p =
  Authorize (Authorization (if partyId p == partyId nobodyParty then minBound else maxBound) p p) Nothing

filterAll :: PGQuery a b => Bool -> a -> a
filterAll True = id
filterAll False = (`unsafeModifyQuery` (++ "WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP"))

lookupAuthorizedParents :: (MonadDB m, MonadHasIdentity c m) => Party -> Bool -> m [Authorize]
lookupAuthorizedParents child al = do
  ident <- peek
  dbQuery $ filterAll al $(selectQuery (selectAuthorizeParent 'child 'ident) "$")

lookupAuthorizedChildren :: (MonadDB m, MonadHasIdentity c m) => Party -> Bool -> m [Authorize]
lookupAuthorizedChildren parent al = do
  ident <- peek
  dbQuery $ filterAll al $(selectQuery (selectAuthorizeChild 'parent 'ident) "$")

lookupAuthorize :: (MonadDB m, MonadHasIdentity c m) => Party -> Party -> m (Maybe Authorize)
lookupAuthorize child parent =
  dbQuery1 $ (\a -> a child parent) <$> $(selectQuery authorizeRow "$WHERE authorize.child = ${partyId child} AND authorize.parent = ${partyId parent}")

lookupAuthorizeParent :: (MonadDB m, MonadHasIdentity c m) => Party -> Id Party -> m (Maybe Authorize)
lookupAuthorizeParent child parent = do
  ident <- peek
  dbQuery1 $ $(selectQuery (selectAuthorizeParent 'child 'ident) "$WHERE authorize.parent = ${parent}")

lookupAuthorization :: (MonadDB m, MonadHasIdentity c m) => Party -> Party -> m Authorization
lookupAuthorization child parent 
  | partyId child == partyId parent = return $ authorization $ selfAuthorize child
  | otherwise = do
    auth <- peek
    if partyId (view auth) == partyId child && partyId parent == partyId rootParty
      then return $ Authorization (siteAccess auth) child parent
      else fromMaybe (Authorization mempty child parent) <$>
        dbQuery1 ((\a -> a child parent) <$> $(selectQuery authorizationRow "!$WHERE authorize_view.child = ${partyId child} AND authorize_view.parent = ${partyId parent}"))

changeAuthorize :: (MonadAudit c m) => Authorize -> m ()
changeAuthorize auth = do
  ident <- getAuditIdentity
  (r, _) <- updateOrInsert
    $(updateAuthorize 'ident 'auth)
    $(insertAuthorize 'ident 'auth)
  when (r /= 1) $ fail $ "changeAuthorize: " ++ show r ++ " rows"

removeAuthorize :: (MonadAudit c m) => Authorize -> m Bool
removeAuthorize auth = do
  ident <- getAuditIdentity
  dbExecute1 $(deleteAuthorize 'ident 'auth)

authorizeJSON :: Authorize -> JSON.Object
authorizeJSON Authorize{..} = accessJSON (authorizeAccess authorization)
  JSON..+? (("expires" JSON..=) <$> authorizeExpires)
