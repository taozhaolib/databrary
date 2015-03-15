module Databrary.Controller.Permission
  ( checkPermission
  ) where

import Control.Monad.IO.Class (MonadIO)

import Databrary.Has (Has, view)
import Databrary.Model.Permission
import Databrary.Action

checkPermission :: (MonadAction c m, MonadIO m, Has Permission a) => Permission -> a -> m a
checkPermission p o = do
  guardAction (view o >= p) forbiddenResponse
  return o
