{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission 
  ( module Databrary.Model.Permission.Types
  , checkPermission
  ) where

import Control.Has (peeks)
import Databrary.Identity.Types
import Databrary.Model.Permission.Types

checkPermission :: IdentityM c m => Bool -> m Bool
checkPermission True = return True
checkPermission False = peeks identitySuperuser
