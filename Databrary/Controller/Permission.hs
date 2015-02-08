module Databrary.Controller.Permission
  ( checkPermission
  ) where

import Control.Monad.IO.Class (MonadIO)

import Control.Has (Has)
import Databrary.Model.Permission
import Databrary.Action
import Databrary.Identity

checkPermission :: (ActionM c m, MonadHasIdentity c m, MonadIO m, Has Permission a) => Permission -> a -> m a
checkPermission p o = do
  c <- testPermission p o
  guardAction c forbiddenResponse
  return o
