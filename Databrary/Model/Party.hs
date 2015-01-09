{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}
module Databrary.Model.Party 
  ( Party(..)
  , changeParty
  ) where

import Data.Int (Int32)
import qualified Data.Text as T

import Databrary.Snaplet.PG
import Databrary.Model.Id
import Databrary.Model.Inet
import Databrary.Model.SQL.Party

useTPG

data Party = Party
  { partyId :: Id Party
  , partyName :: T.Text
  , partyAffiliation :: Maybe T.Text
  , partyURL :: T.Text
  }

instance HasId Party where
  key = partyId
  kind _ = "party"

changeParty :: HasPG m => Party -> m ()
changeParty Party{..} = pgExecute1 $(changeQuery)
  where
  clientIP = Inet ""
  identityId = (-1 :: Int32)
