{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission 
  ( module Databrary.Model.Permission.Types
  , permissionVIEW
  , permissionPRIVATE
  , readPermission
  , readClassification
  , testPermission
  , accessJSON
  ) where

import Control.Has (Has, see, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Model.Identity.Types
import Databrary.Model.Permission.Types

-- |Level at which things become visible.
permissionVIEW :: Permission
permissionVIEW = PermissionPUBLIC

-- |Alias for READ. Grants full access to private data, bypassing consent permissions.
permissionPRIVATE :: Permission
permissionPRIVATE = PermissionREAD

-- |The necessary permission level to read a data object with the given classification.
-- Equivalent to the SQL function read_permission.
readPermission :: Classification -> Maybe Consent -> Permission
readPermission ClassificationPRIVATE _  = permissionPRIVATE
readPermission ClassificationPUBLIC  _  = PermissionPUBLIC
readPermission _ (Just ConsentPUBLIC)   = PermissionPUBLIC
readPermission ClassificationSHARED  _  = PermissionSHARED
readPermission _ (Just ConsentEXCERPTS) = PermissionSHARED
readPermission _ (Just ConsentSHARED)   = PermissionSHARED
readPermission _ _                      = permissionPRIVATE

-- |The most restrictive data classification level that the current user may access under the given permission and consent level.
-- Equivalent to the SQL function read_classification.
readClassification :: Permission -> Maybe Consent -> Maybe Classification
readClassification PermissionNONE _   = Nothing
readClassification PermissionPUBLIC (Just ConsentPUBLIC) 
                                      = Just ClassificationRESTRICTED
readClassification PermissionPUBLIC _ = Just ClassificationSHARED
readClassification PermissionSHARED (Just c) | c >= ConsentSHARED 
                                      = Just ClassificationRESTRICTED
readClassification PermissionSHARED _ = Just ClassificationSHARED
readClassification _ _                = Just ClassificationPRIVATE

testPermission :: (MonadHasIdentity c m, Has Permission a) => Permission -> a -> m Bool
testPermission p o
  | see o >= p = return True
  | otherwise = peeks identitySuperuser

accessJSON :: Access -> JSON.Object
accessJSON Access{..} = JSON.object
  [ "site" JSON..= _accessSite
  , "member" JSON..= _accessMember
  ]
