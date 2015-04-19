{-# LANGUAGE TemplateHaskell, OverloadedStrings, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Consent.Types 
  ( Consent(..)
  ) where

import Language.Haskell.TH.Lift (deriveLift)

import Databrary.Service.DB
import Databrary.Model.Enum

useTPG

makeDBEnum "consent" "Consent"

deriveLift ''Consent
