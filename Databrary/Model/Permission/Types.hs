{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission.Types 
  ( Permission(..)
  , Consent(..)
  , Classification(..)
  , Access(..)
  , accessSite, accessMember, accessPermission
  ) where

import Control.Lens (Lens', makeLensesFor)
import Control.Monad (join)
import Data.Monoid (Monoid(..))
import Language.Haskell.TH.Lift (deriveLiftMany)

import Control.Has (Has(..))
import Databrary.DB (useTPG)
import Databrary.Enum

useTPG

makeDBEnum "permission" "Permission"
makeDBEnum "consent" "Consent"
makeDBEnum "classification" "Classification"

deriveLiftMany [''Permission, ''Consent, ''Classification]

data Access = Access
  { _accessSite :: !Permission
  , _accessMember :: !Permission
  }

makeLensesFor [("_accessSite", "accessSite'"), ("_accessMember", "accessMember'")] ''Access
accessPermission' :: Lens' Access Permission
accessPermission' f (Access s m) = fmap (join Access) $ f $ min s m

accessSite, accessMember, accessPermission :: Has Access a => Lens' a Permission
accessSite = view . accessSite'
accessMember = view . accessMember'
accessPermission = view . accessPermission'

instance Bounded Access where
  minBound = Access minBound minBound
  maxBound = Access maxBound maxBound

instance Monoid Access where
  mempty = Access PermissionNONE PermissionNONE
  mappend (Access s1 m1) (Access s2 m2) = Access (max s1 s2) (max m1 m2)

