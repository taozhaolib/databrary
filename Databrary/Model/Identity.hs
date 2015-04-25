{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Identity
  ( module Databrary.Model.Identity.Types
  , determineIdentity
  , foldIdentity
  , maybeIdentity
  , identityJSON
  ) where

import Data.Maybe (catMaybes)

import Databrary.Ops
import Databrary.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Model.Token
import Databrary.HTTP.Request
import Databrary.Service.Types
import Databrary.Service.DB
import Databrary.HTTP.Cookie
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Identity.Types

determineIdentity :: (MonadHasService c m, MonadHasRequest c m, MonadDB m) => m Identity
determineIdentity = do
  c <- getSignedCookie "session"
  s <- flatMapM lookupSession c
  return $ maybe UnIdentified Identified s

foldIdentity :: a -> (Session -> a) -> Identity -> a
foldIdentity _ i (Identified s) = i s
foldIdentity u _ _ = u

maybeIdentity :: (MonadHasIdentity c m) => m a -> (Session -> m a) -> m a
maybeIdentity u i = foldIdentity u i =<< peek

identityJSON :: Identity -> JSON.Object
identityJSON i = partyJSON (view i) JSON..++ catMaybes
  [ Just $ "access" JSON..= accessJSON (view i)
  , identitySuperuser i ?> ("superuser" JSON..= True)
  ]
