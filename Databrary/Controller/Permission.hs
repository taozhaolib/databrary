module Databrary.Controller.Permission
  ( checkPermission
  , authAccount
  ) where

import Control.Monad.IO.Class (MonadIO)

import Databrary.Has (Has, view, peek)
import Databrary.Model.Permission
import Databrary.Model.Party
import Databrary.Model.Identity
import Databrary.Action

checkPermission :: (MonadAction c m, MonadIO m, Has Permission a) => Permission -> a -> m a
checkPermission p o = do
  guardAction (view o >= p) forbiddenResponse
  return o

authAccount :: AuthActionM Account
authAccount = do
  ident <- peek
  case ident of
    UnIdentified -> result =<< forbiddenResponse
    Identified s -> return $ view s
    ReIdentified u -> return $ view u
