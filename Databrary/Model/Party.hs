{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Party 
  ( module Databrary.Model.Types.Party
  , changeParty
  , getParty
  ) where

import Data.Int (Int32)

import Databrary.DB
import Databrary.Model.Id
import Databrary.Model.Inet
import Databrary.Model.Types.Party
import Databrary.Model.SQL.Party
import Databrary.Model.SQL (selectQuery)

useTPG

changeParty :: (Monad m, MonadDB m) => Party -> m ()
changeParty Party{..} = dbExecute1 $(changeQuery)
  where
  clientIP = Inet ""
  identityId = (-1 :: Int32)

getParty :: (Monad m, MonadDB m) => Id Party -> m (Maybe Party)
getParty i = dbQuery1 $(selectQuery partySelector "WHERE party.id = ${i}")

getAccount :: (Monad m, MonadDB m) => Id Party -> m (Maybe Account)
getAccount i = dbQuery1 $(selectQuery accountSelector "WHERE account.id = ${i}")
