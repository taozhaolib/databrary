{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, ConstraintKinds #-}
module Databrary.Model.Party 
  ( module Databrary.Model.Types.Party
  , nobodyParty
  , rootParty
  , changeParty
  , lookupParty
  , lookupAccount
  ) where

import Databrary.DB
import Databrary.Model.Id
import Databrary.Model.SQL.Party
import Databrary.Model.SQL (selectQuery)
import Databrary.Model.Audit
import Databrary.Model.Types.Party

useTPG

nobodyParty :: Party
nobodyParty = Party
  { partyId = Id (-1)
  , partyName = "Everybody"
  , partyAffiliation = Nothing
  , partyURL = Nothing
  , partyAccount = Nothing
  }

rootParty :: Party
rootParty = Party
  { partyId = Id 0
  , partyName = "Databrary"
  , partyAffiliation = Nothing
  , partyURL = Nothing
  , partyAccount = Nothing
  }

changeParty :: AuditM m => Party -> m ()
changeParty p = dbExecute1 =<< $(changeQuery "p")

lookupParty :: DBM m => Id Party -> m (Maybe Party)
lookupParty i = dbQuery1 $(selectQuery partySelector "WHERE party.id = ${i}")

lookupAccount :: DBM m => Id Party -> m (Maybe Account)
lookupAccount i = dbQuery1 $(selectQuery accountSelector "WHERE account.id = ${i}")
