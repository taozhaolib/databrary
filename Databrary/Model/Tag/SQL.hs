{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Tag.SQL
  ( selectTag
  , insertTagUse
  , deleteTagUse
  , selectSlotTagCoverage
  ) where

import Data.Int (Int32)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Typed.Query (makePGQuery, simpleQueryFlags)
import Database.PostgreSQL.Typed.Dynamic (pgLiteralString)
import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.SQL.Info
import Databrary.Model.Tag.Types
import Databrary.Model.Container.Types
import Databrary.Model.Segment
import Databrary.Model.Slot.Types

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

makeTagCoverage :: Int32 -> [Maybe Segment] -> [Maybe Segment] -> [Maybe Segment] -> Tag -> Container -> TagCoverage
makeTagCoverage w s k v t c = TagCoverage t c w (segs s) (segs k) (segs v) where
  segs = map $ fromMaybe (error "NULL tag segment")

tagCoverageColumns :: TH.Name -- ^ @'Party'@
  -> TH.Q [(String, String)]
tagCoverageColumns acct = do
  tagUseOID <- lookupTableOID "tag_use"
  keywordUseOID <- lookupTableOID "keyword_use"
  return
    [ ("weight", "count(*)::integer")
    , ("coverage", "segments_union(segment)")
    , ("keywords", "segments_union(CASE WHEN tableoid = " ++ pgLiteralString keywordUseOID ++ " THEN segment ELSE 'empty' END)")
    , ("votes", "segments_union(CASE WHEN tableoid = " ++ pgLiteralString tagUseOID ++ " AND who = ${partyId " ++ nameRef acct ++ "} THEN segment ELSE 'empty' END)")
    ]

selectTagCoverage :: TH.Name -- ^ @'Party'@
  -> String -- query
  -> TH.Q Selector -- ^ @'Tag' -> 'Container' -> 'TagCoverage'@
selectTagCoverage acct q = do
  cols <- tagCoverageColumns acct
  return $ selector
    ("(SELECT tag," ++ intercalate "," (map (\(a, s) -> s ++ " AS " ++ a) cols)
      ++ " FROM tag_use " ++ q ++ " GROUP BY tag) AS tag_coverage")
    $ OutputJoin False 'makeTagCoverage $ map (OutputExpr . ("tag_coverage." ++) . fst) cols

selectSlotTagCoverage :: TH.Name -- ^ @'Party'@
  -> TH.Name -- ^ @'Slot'
  -> TH.Q Selector -- ^ @'TagCoverage'@
selectSlotTagCoverage acct slot = do
  tagCoverage <- selectTagCoverage acct $ "WHERE container = ${containerId $ slotContainer " ++ ss ++ "} AND segment && ${slotSegment " ++ ss ++ "}"
  return $ selectMap
    (`TH.AppE` (TH.VarE 'slotContainer `TH.AppE` TH.VarE slot)) $ selectJoin '($) $
    [ tagCoverage
    , joinOn "tag_coverage.tag = tag.id" selectTag 
    ]
  where ss = nameRef slot
