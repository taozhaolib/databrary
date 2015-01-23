{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission 
  ( module Databrary.Model.Types.Permission
  , checkPermission
  ) where

import Control.Has (peeks)
import Databrary.Types.Identity
import Databrary.Model.Types.Permission

checkPermission :: IdentityM c m => Bool -> m Bool
checkPermission True = return True
checkPermission False = peeks identitySuperuser
