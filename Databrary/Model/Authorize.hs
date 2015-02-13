{-# LANGUAGE TemplateHaskell, RecordWildCards, OverloadedStrings #-}
module Databrary.Model.Authorize
  ( module Databrary.Model.Authorize.Types
  , getAuthorizedChildren
  , getAuthorizedParents
  , authorizeJSON
  ) where

import Control.Applicative ((<$>))
import Database.PostgreSQL.Typed.Query (PGQuery, unsafeModifyQuery)

import Control.Has (peek)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Permission
import Databrary.Model.Party.Types
import Databrary.Model.Identity.Types
import Databrary.Model.Authorize.Types
import Databrary.Model.Authorize.SQL

useTPG

filterAll :: PGQuery a b => Bool -> a -> a
filterAll True = id
filterAll False = (`unsafeModifyQuery` (++ "WHERE expires IS NULL OR expires > CURRENT_TIMESTAMP"))

getAuthorizedParents :: (DBM m, MonadHasIdentity c m) => Party -> Bool -> m [Authorize]
getAuthorizedParents child al = do
  ident <- peek
  dbQuery $ filterAll al $(selectQuery (selectAuthorizeParent 'child 'ident) "")

getAuthorizedChildren :: (DBM m, MonadHasIdentity c m) => Party -> Bool -> m [Authorize]
getAuthorizedChildren parent al = do
  ident <- peek
  dbQuery $ filterAll al $(selectQuery (selectAuthorizeChild 'parent 'ident) "")

authorizeJSON :: Authorize -> JSON.Object
authorizeJSON Authorize{..} = accessJSON (authorizeAccess authorization)
  JSON..+? (("expires" JSON..=) <$> authorizeExpires)
