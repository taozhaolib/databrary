{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission 
  ( Permission(..)
  , Consent(..)
  , Classification(..)
  , HasPermission(..)
  ) where

import Database.PostgreSQL.Typed.Enum (makePGEnum)

import Databrary.DB (useTPG)

useTPG

makePGEnum "permission" "Permission" ("Permission" ++)
makePGEnum "consent" "Consent" ("Consent" ++)
makePGEnum "classification" "Classification" ("Classification" ++)

class HasPermission a where
  permission :: a -> Permission

instance HasPermission Permission where
  permission = id
