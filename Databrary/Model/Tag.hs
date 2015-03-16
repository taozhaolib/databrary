{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Tag
  ( module Databrary.Model.Tag.Types
  , lookupTag
  , addTag
  , addTagUse
  , removeTagUse
  ) where

import Control.Monad (guard)
import Database.PostgreSQL.Typed (pgSQL)

import Databrary.Ops
import Databrary.DB
import Databrary.Model.SQL
import Databrary.Model.Party.Types
import Databrary.Model.Container.Types
import Databrary.Model.Slot.Types
import Databrary.Model.Tag.Types
import Databrary.Model.Tag.SQL

useTPG

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
