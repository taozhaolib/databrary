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
import Databrary.Model.Release.Types
import Databrary.Model.Segment
import Databrary.Model.AssetSlot.Types
import Databrary.Model.AssetSegment.Types

makeExcerpt :: Segment -> Maybe Release -> AssetSlot -> Excerpt
makeExcerpt s r a = newExcerpt a s r

excerptRow :: Selector -- ^ @'AssetSlot' -> 'Excerpt'@
excerptRow = selectColumns 'makeExcerpt "excerpt" ["segment", "release"]

selectAssetSlotExcerpt :: Selector -- ^ @'AssetSlot' -> 'Excerpt'@
selectAssetSlotExcerpt = excerptRow

excerptKeys :: String -- ^ @'Excerpt'@
  -> [(String, String)]
excerptKeys o =
  [ ("asset", "${assetId $ slotAsset $ segmentAsset $ excerptAsset " ++ o ++ "}")
  , ("segment", "${assetSegment $ excerptAsset " ++ o ++ "}")
  ]

excerptSets :: String -- ^ @'Excerpt'@
  -> [(String, String)]
excerptSets o =
  [ ("release", "${excerptRelease " ++ o ++ "}")
  ]

insertExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Excerpt'@
  -> TH.ExpQ
insertExcerpt ident o = auditInsert ident "excerpt"
  (excerptKeys os ++ excerptSets os)
  Nothing
  where os = nameRef o

updateExcerpt :: TH.Name -- ^ @'AuditIdentity'@
  -> TH.Name -- ^ @'Excerpt'@
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
