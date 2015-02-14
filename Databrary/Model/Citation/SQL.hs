{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Citation.SQL
  ( selectVolumeCitation
  , selectVolumeLink
  ) where

import Data.Int (Int16)
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH
import qualified Network.URI as URI

import Databrary.Model.SQL
import Databrary.Model.Citation.Types

makeCitation :: T.Text -> Maybe String -> Maybe Int16 -> Maybe T.Text -> Citation
makeCitation h u y t = Citation h t (URI.parseURI =<< u) y

citationRow :: Selector -- ^ @Maybe 'T.Text' -> 'Citation'@
citationRow = selectColumns 'makeCitation "volume_citation" ["head", "url", "year"]

selectVolumeCitation :: Selector -- ^ @Maybe 'T.Text' -> 'Citation'@
selectVolumeCitation = citationRow

linkRow :: Selector -- ^ @'Citation'@
linkRow = selectColumns 'makeCitation "volume_link" ["head", "url"]

selectVolumeLink :: Selector -- ^ @'Citation'@
selectVolumeLink = selectMap ((`TH.AppE` TH.ConE 'Nothing) . (`TH.AppE` TH.ConE 'Nothing))
  linkRow

