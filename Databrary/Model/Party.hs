{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Party 
  ( module Databrary.Model.Types.Party
  , nobodyParty
  , rootParty
  , changeParty
  , lookupParty
  , lookupAccount
  ) where

import Data.Int (Int32)

import Databrary.DB
import Databrary.Model.Id
import Databrary.Model.Inet
import Databrary.Model.Types.Party
import Databrary.Model.SQL.Party
import Databrary.Model.SQL (selectQuery)

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

changeParty :: DBM m => Party -> m ()
changeParty Party{..} = dbExecute1 $(changeQuery)
  where
  clientIP = Inet ""
  identityId = (-1 :: Int32)

lookupParty :: DBM m => Id Party -> m (Maybe Party)
lookupParty i = dbQuery1 $(selectQuery partySelector "WHERE party.id = ${i}")

lookupAccount :: DBM m => Id Party -> m (Maybe Account)
lookupAccount i = dbQuery1 $(selectQuery accountSelector "WHERE account.id = ${i}")
