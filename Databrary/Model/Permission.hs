{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Permission 
  ( Permission(..)
  , Consent(..)
  , Classification(..)
  , HasPermission(..)
  ) where

import Database.PostgreSQL.Typed.Enum (makeEnum)

import Databrary.App

useTPG

makeEnum "permission" "Permission" ("Permission" ++)
makeEnum "consent" "Consent" ("Consent" ++)
makeEnum "classification" "Classification" ("Classification" ++)

class HasPermission a where
  permission :: a -> Permission

instance HasPermission Permission where
  permission = id
