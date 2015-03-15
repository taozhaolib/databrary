{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Tag
  ( module Databrary.Model.Tag.Types
  , validateTag
  , lookupTag
  , addTag
  , addTagUse
  , removeTagUse
  ) where

import Control.Monad (guard)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Database.PostgreSQL.Typed (pgSQL)
import qualified Text.Regex.Posix as Regex

import Databrary.Ops
import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Tag.SQL

useTPG

validTag :: Regex.Regex
validTag = Regex.makeRegex
  "^[a-z][-a-z ]+[a-z]$"

validateTag :: BS.ByteString -> Maybe TagName
validateTag t = Regex.matchTest validTag tt ?> TagName tt where
  tt = BSC.unwords $ BSC.words t

lookupTag :: DBM m => TagName -> m (Maybe Tag)
lookupTag n =
  dbQuery1 $(selectQuery selectTag "$WHERE tag.name = ${n}::varchar")

addTag :: DBM m => TagName -> m Tag
addTag n =
  dbQuery1' $ (`Tag` n) <$> [pgSQL|!SELECT get_tag(${n})|]

addTagUse :: DBM m => TagUse -> m Bool
addTagUse t =
  either (const False) ((0 <) . fst) <$> dbTryQuery (guard . isExclusionViolation)
    (if tagKeyword t
      then $(insertTagUse True 't)
      else $(insertTagUse False 't))

removeTagUse :: DBM m => TagUse -> m Bool
removeTagUse t =
  (0 <) <$> dbExecute
    (if tagKeyword t 
      then $(deleteTagUse True 't)
      else $(deleteTagUse False 't))
