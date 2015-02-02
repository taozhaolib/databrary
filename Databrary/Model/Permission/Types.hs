{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission.Types 
  ( Permission(..)
  , Consent(..)
  , Classification(..)
  ) where

import Database.PostgreSQL.Typed.Enum (makePGEnum)

import Databrary.DB (useTPG)
import qualified Databrary.JSON as JSON

useTPG

makePGEnum "permission" "Permission" ("Permission" ++)
makePGEnum "consent" "Consent" ("Consent" ++)
makePGEnum "classification" "Classification" ("Classification" ++)

instance JSON.ToJSON Permission where
  toJSON = JSON.toJSON . fromEnum
instance JSON.FromJSON Permission where
  parseJSON = JSON.parseEnum "permission"

instance JSON.ToJSON Consent where
  toJSON = JSON.toJSON . fromEnum
instance JSON.FromJSON Consent where
  parseJSON = JSON.parseEnum "consent"

instance JSON.ToJSON Classification where
  toJSON = JSON.toJSON . fromEnum
instance JSON.FromJSON Classification where
  parseJSON = JSON.parseEnum "classification"
