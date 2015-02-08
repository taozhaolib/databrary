{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Permission.Types 
  ( Permission(..)
  , Consent(..)
  , Classification(..)
  ) where

import Language.Haskell.TH.Lift (deriveLiftMany)

import Databrary.DB (useTPG)
import Databrary.Enum

useTPG

makeDBEnum "permission" "Permission"
makeDBEnum "consent" "Consent"
makeDBEnum "classification" "Classification"

deriveLiftMany [''Permission, ''Consent, ''Classification]
