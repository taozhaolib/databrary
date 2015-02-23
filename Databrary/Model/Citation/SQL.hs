{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Citation.SQL
  ( selectVolumeCitation
  , selectVolumeLink
  ) where

import qualified Language.Haskell.TH as TH

import Databrary.Model.SQL.Select
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

