{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Party 
  ( module Databrary.Model.Types.Party
  , changeParty
  , getParty
  , testParty
  ) where

import Data.Int (Int32)
import Snap.Core (writeText)

import Databrary.Snaplet.PG
import Databrary.App
import Databrary.Model.Id
import Databrary.Model.Inet
import Databrary.Model.Types.Party
import Databrary.Model.SQL.Party
import Databrary.Model.SQL (selectQuery)

useTPG

changeParty :: Party -> AppHandler ()
changeParty Party{..} = pgExecute1 $(changeQuery)
  where
  clientIP = Inet ""
  identityId = (-1 :: Int32)

getParty :: Id Party -> AppHandler (Maybe Party)
getParty i = pgQuery1 $(selectQuery partySelector "WHERE party.id = ${i}")

getAccount :: Id Party -> AppHandler (Maybe Account)
getAccount i = pgQuery1 $(selectQuery accountSelector "WHERE account.id = ${i}")

testParty :: Id Party -> AppHandler ()
testParty i = do
  p <- getParty i
  writeText $ maybe "not found" partyName p
