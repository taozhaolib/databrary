{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission 
  ( module Databrary.Model.Permission.Types
  , testPermission
  ) where

import Control.Has (Has, see, peeks)
import Databrary.Identity.Types
import Databrary.Model.Permission.Types

testPermission :: (MonadHasIdentity c m, Has Permission a) => Permission -> a -> m Bool
testPermission p o
  | see o >= p = return True
  | otherwise = peeks identitySuperuser
