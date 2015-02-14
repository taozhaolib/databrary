{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission 
  ( module Databrary.Model.Permission.Types
  , permissionVIEW
  , permissionPRIVATE
  , readPermission
  , readClassification
  , dataPermission
  , accessJSON
  ) where

import Control.Has (Has, see)
import qualified Databrary.JSON as JSON
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

-- |The effective permission for data objects with the given attributes, effectively collapsing selective read permissions to READ or NONE.
dataPermission :: Has Permission a => a -> Classification -> Maybe Consent -> Permission
dataPermission o c = dp (see o) . readPermission c where
  dp p r
    | p >= PermissionREAD = p
    | p >= r = PermissionREAD
    | otherwise = PermissionNONE

accessJSON :: Access -> JSON.Object
accessJSON Access{..} = JSON.object
  [ "site" JSON..= _accessSite
  , "member" JSON..= _accessMember
  ]
