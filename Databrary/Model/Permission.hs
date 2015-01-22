{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, ConstraintKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission 
  ( module Databrary.Model.Types.Permission
  , checkPermission
  ) where

import Control.Monad.Has (pulls)
import Databrary.Types.Identity
import Databrary.Model.Types.Permission

checkPermission :: IdentityM c m => Bool -> m Bool
checkPermission True = return True
checkPermission False = pulls identitySuperuser
