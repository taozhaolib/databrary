{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Tag.SQL
  ( selectTag
  , insertTagUse
  , deleteTagUse
  ) where

import Database.PostgreSQL.Typed.Query (makePGQuery, simpleQueryFlags)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Tag.Types

tagRow :: Selector -- ^ @'Tag'@
tagRow = selectColumns 'Tag "tag" ["id", "name"]

selectTag :: Selector -- ^ @'Tag'@
selectTag = tagRow

tagUseTable :: Bool -> String
tagUseTable False = "tag_use"
tagUseTable True = "keyword_use"

insertTagUse :: Bool -- ^ keyword
  -> TH.Name -- ^ @'TagUse'@
  -> TH.ExpQ
insertTagUse keyword o = makePGQuery simpleQueryFlags $
  "INSERT INTO " ++ tagUseTable keyword ++ " (tag, container, segment, who) VALUES (${tagId $ tag " ++ os ++ "}, ${containerId $ slotContainer $ tagSlot  " ++ os ++ "}, ${slotSegment $ tagSlot  " ++ os ++ "}, ${partyId $ accountParty $ tagWho  " ++ os ++ "})"
  where os = nameRef o

deleteTagUse :: Bool -- ^ keyword
  -> TH.Name -- ^ @'TagUse'@
  -> TH.ExpQ
deleteTagUse keyword o = makePGQuery simpleQueryFlags $
  "DELETE FROM " ++ tagUseTable keyword ++ " WHERE tag = ${tagId $ tag " ++ os ++ "} AND container = ${containerId $ slotContainer $ tagSlot " ++ os ++ "} AND segment <@ ${slotSegment $ tagSlot " ++ os ++ "}"
  ++ (if keyword then "" else " AND who = ${partyId $ accountParty $ tagWho " ++ os ++ "}")
  where os = nameRef o
