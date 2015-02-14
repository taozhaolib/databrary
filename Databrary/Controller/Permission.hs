module Databrary.Controller.Permission
  ( checkPermission
  ) where

import Control.Monad.IO.Class (MonadIO)

import Control.Has (Has, see)
import Databrary.Model.Permission
import Databrary.Action

checkPermission :: (ActionM c m, MonadIO m, Has Permission a) => Permission -> a -> m a
checkPermission p o = do
  guardAction (see o >= p) forbiddenResponse
  return o
