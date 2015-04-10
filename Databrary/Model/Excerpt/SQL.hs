{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Excerpt.SQL
  ( selectAssetSlotExcerpt
  , insertExcerpt
  , updateExcerpt
  , deleteExcerpt
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Permission.Types
import Databrary.Model.Segment
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSegment.Types

makeExcerpt :: Segment -> Classification -> AssetSlot -> AssetSegment
makeExcerpt s c a = AssetSegment a s (Just c)

excerptRow :: Selector -- ^ @'AssetSlot' -> 'AssetSegment'@
excerptRow = selectColumns 'makeExcerpt "excerpt" ["segment", "classification"]

selectAssetSlotExcerpt :: Selector -- ^ @'AssetSlot' -> 'AssetSegment'@
selectAssetSlotExcerpt = excerptRow

excerptKeys :: String -- ^ @'AssetSegment'@
  -> [(String, String)]
excerptKeys o =
  [ ("asset", "${assetId $ slotAsset $ segmentAsset " ++ o ++ "}")
  , ("segment", "${assetSegment " ++ o ++ "}")
  ]

excerptSets :: String -- ^ @'AssetSegment'@
  -> [(String, String)]
excerptSets o =
  [ ("classification", "${assetSegmentExcerpt " ++ o ++ "}")
  ]

insertExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSegment'@
  -> TH.ExpQ
insertExcerpt ident o = auditInsert ident "excerpt"
  (excerptKeys os ++ excerptSets os)
  Nothing
  where os = nameRef o

updateExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSegment'@
  -> TH.ExpQ
updateExcerpt ident o = auditUpdate ident "excerpt"
  (excerptSets os)
  (whereEq $ excerptKeys os)
  Nothing
  where os = nameRef o

deleteExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'AssetSegment'@
  -> TH.ExpQ
deleteExcerpt ident o = auditDelete ident "excerpt"
  ("asset = ${assetId $ slotAsset $ segmentAsset " ++ os ++ "} AND segment <@ ${assetSegment " ++ os ++ "}")
  Nothing
  where os = nameRef o
