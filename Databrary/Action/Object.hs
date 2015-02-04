module Databrary.Action.Object
  ( checkObject
  ) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)

import Control.Has (Has, see)
import Databrary.Model.Permission
import Databrary.Action
import Databrary.Identity

checkObject :: (ActionM c m, IdentityM c m, MonadIO m, Has Permission a) => Permission -> m (Maybe a) -> m a
checkObject p g = do
  o <- maybeAction =<< g
  c <- checkPermission (see o >= p)
  unless c $ result =<< forbiddenResponse
  return o
