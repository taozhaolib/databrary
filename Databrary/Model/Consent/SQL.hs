{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Consent.SQL
  ( consentRow
  , insertConsent
  , updateConsent
  , deleteConsent
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Slot.SQL

consentRow :: Selector -- ^ @'Consent'@
consentRow = selector "slot_consent" "consent"

consentSets :: String -- ^ @'Consent'@
  -> [(String, String)]
consentSets o =
  [ ("consent", "${" ++ o ++ "}")
  ]

insertConsent :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Slot'@
  -> TH.Name -- ^ @'Consent'@
  -> TH.ExpQ
insertConsent ident s c = auditInsert ident "slot_consent"
  (slotKeys (nameRef s) ++ consentSets (nameRef c))
  Nothing

updateConsent :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Slot'@
  -> TH.Name -- ^ @'Consent'@
  -> TH.ExpQ -- ^ @'Consent'@
updateConsent ident s c = auditUpdate ident "slot_consent"
  (consentSets (nameRef c))
  (whereEq $ slotKeys (nameRef s))
  Nothing

deleteConsent :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Slot'@
  -> TH.ExpQ -- ^ @()@
deleteConsent ident s = auditDelete ident "slot_consent"
  (whereEq $ slotKeys (nameRef s))
  Nothing

