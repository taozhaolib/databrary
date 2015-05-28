module Databrary.Controller.Permission
  ( checkPermission
  , checkDataPermission
  , authAccount
  ) where

import Control.Monad.IO.Class (MonadIO)

import Databrary.Has (Has, view, peek)
import Databrary.Model.Permission
import Databrary.Model.Release
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Action

checkPermission :: (MonadAction c m, MonadIO m, Has Permission a) => Permission -> a -> m a
checkPermission p o = do
  guardAction (view o >= p) forbiddenResponse
  return o

checkDataPermission :: (MonadAction c m, MonadIO m, Has Release a, Has Permission a) => a -> m a
checkDataPermission o = do
  guardAction (dataPermission o > PermissionNONE) forbiddenResponse
  return o

authAccount :: AuthActionM Account
authAccount = do
  ident <- peek
  case ident of
    UnIdentified -> result =<< forbiddenResponse
    Identified s -> return $ view s
    ReIdentified u -> return $ view u
