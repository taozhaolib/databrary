{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Authorize
  ( module Databrary.Model.Authorize.Types
  , lookupAuthorizedChildren
  , lookupAuthorizedParents
  , lookupAuthorize
  , setAuthorize
  , removeAuthorize
  , authorizeJSON
  ) where

import Control.Applicative ((<$>))
import Control.Monad (when)
import Database.PostgreSQL.Typed.Query (PGQuery, unsafeModifyQuery)

import Control.Has (peek)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Permission
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Authorize.Types
import Databrary.Model.Authorize.SQL

useTPG

filterAll :: PGQuery a b => Bool -> a -> a
filterAll True = id
filterAll False = (`unsafeModifyQuery` (++ "WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP"))

lookupAuthorizedParents :: (DBM m, MonadHasIdentity c m) => Party -> Bool -> m [Authorize]
lookupAuthorizedParents child al = do
  ident <- peek
  dbQuery $ filterAll al $(selectQuery (selectAuthorizeParent 'child 'ident) "$")

lookupAuthorizedChildren :: (DBM m, MonadHasIdentity c m) => Party -> Bool -> m [Authorize]
lookupAuthorizedChildren parent al = do
  ident <- peek
  dbQuery $ filterAll al $(selectQuery (selectAuthorizeChild 'parent 'ident) "$")

lookupAuthorize :: (DBM m, MonadHasIdentity c m) => Party -> Party -> m (Maybe Authorize)
lookupAuthorize child parent =
  dbQuery1 $ (\a -> a child parent) <$> $(selectQuery authorizeRow "$WHERE authorize.child = ${partyId child} AND authorize.parent = ${partyId parent}")

setAuthorize :: (AuditM c m) => Authorize -> m ()
setAuthorize auth = do
  ident <- getAuditIdentity
  r <- updateOrInsert
    $(updateAuthorize 'ident 'auth)
    $(insertAuthorize 'ident 'auth)
  when (r /= 1) $ fail $ "setAuthorize: " ++ show r ++ " rows"

removeAuthorize :: (AuditM c m) => Authorize -> m Bool
removeAuthorize auth = do
  ident <- getAuditIdentity
  (0 <) <$> dbExecute $(deleteAuthorize 'ident 'auth)

authorizeJSON :: Authorize -> JSON.Object
authorizeJSON Authorize{..} = accessJSON (authorizeAccess authorization)
  JSON..+? (("expires" JSON..=) <$> authorizeExpires)
