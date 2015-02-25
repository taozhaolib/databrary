{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Citation.SQL
  ( selectVolumeCitation
  , insertVolumeCitation
  , updateVolumeCitation
  , deleteVolumeCitation
  , selectVolumeLink
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
import Databrary.Model.Audit.SQL
import Databrary.Model.Citation.Types

citationRow :: Selector -- ^ @Maybe 'T.Text' -> 'Citation'@
citationRow = selectColumns 'Citation "volume_citation" ["head", "url", "year"]

selectVolumeCitation :: Selector -- ^ @Maybe 'T.Text' -> 'Citation'@
selectVolumeCitation = citationRow

linkRow :: Selector -- ^ @'Citation'@
linkRow = selectColumns 'Citation "volume_link" ["head", "url"]

selectVolumeLink :: Selector -- ^ @'Citation'@
selectVolumeLink = selectMap ((`TH.AppE` TH.ConE 'Nothing) . (`TH.AppE` TH.ConE 'Nothing))
  linkRow

volumeKeys :: String -- ^ @'Volume'@
  -> [(String, String)]
volumeKeys v =
  [ ("volume", "${volumeId " ++ v ++ "}") ]

citationSets :: String -- ^ @'Citation'@
  -> [(String, String)]
citationSets c =
  [ ("head", "${citationHead " ++ c ++ "}")
  , ("url", "${citationURL " ++ c ++ "}")
  , ("year", "${citationYear " ++ c ++ "}")
  ]

insertVolumeCitation :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Volume'@
  -> TH.Name -- ^ @'Citation'@
  -> TH.ExpQ -- ^ ()
insertVolumeCitation ident v c = auditInsert ident "volume_citation"
  (volumeKeys (nameRef v) ++ citationSets (nameRef c))
  Nothing

updateVolumeCitation :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Volume'@
  -> TH.Name -- ^ @'Citation'@
  -> TH.ExpQ -- ^ ()
updateVolumeCitation ident v c = auditUpdate ident "volume_citation"
  (citationSets (nameRef c))
  (whereEq $ volumeKeys (nameRef v))
  Nothing

deleteVolumeCitation :: TH.Name -- ^ @'AuditIdentity'
  -> TH.Name -- ^ @'Volume'@
  -> TH.ExpQ -- ^ ()
deleteVolumeCitation ident v = auditDelete ident "volume_citation"
  (whereEq $ volumeKeys (nameRef v))
  Nothing
