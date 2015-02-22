{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Identity
  ( module Databrary.Model.Identity.Types
  , determineIdentity
  , foldIdentity
  , maybeIdentity
  , identityJSON
  ) where

import Data.Maybe (catMaybes)

import Control.Applicative.Ops
import Control.Has (peek, view)
import qualified Databrary.JSON as JSON
import Databrary.Model.Token
import Databrary.Action.Request
import Databrary.Resource
import Databrary.DB
import Databrary.Web.Cookie
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Identity.Types

determineIdentity :: (MonadHasResource c m, MonadHasRequest c m, DBM m) => m Identity
determineIdentity = do
  c <- getSignedCookie "session"
  s <- maybe (return Nothing) lookupSession c
  return $ maybe UnIdentified Identified s

foldIdentity :: a -> (SessionToken -> a) -> Identity -> a
foldIdentity u _ UnIdentified = u
foldIdentity _ i (Identified s) = i s

maybeIdentity :: (MonadHasIdentity c m) => m a -> (SessionToken -> m a) -> m a
maybeIdentity u i = foldIdentity u i =<< peek

identityJSON :: Identity -> JSON.Object
identityJSON i = partyJSON (view i) JSON..++ catMaybes
  [ Just ("access" JSON..= accessJSON (view i))
  , identitySuperuser i ?> ("superuser" JSON..= True)
  ]
