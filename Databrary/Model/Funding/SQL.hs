{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Funding.SQL
  ( selectFunder
  , selectVolumeFunding
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Databrary.Model.SQL.Select
import Databrary.Model.Funding.Types

funderRow :: Selector -- ^ @'Funder'@
funderRow = selectColumns 'Funder "funder" ["fundref_id", "name"]

selectFunder :: Selector -- ^ @'Funder'@
selectFunder = funderRow

makeFunding :: [Maybe T.Text] -> Funder -> Funding
makeFunding a f = Funding f (map (fromMaybe (error "NULL funding.award")) a)

fundingRow :: Selector -- ^ @'Funder' -> 'Funding'@
fundingRow = selectColumns 'makeFunding "volume_funding" ["awards"]

selectVolumeFunding :: Selector -- ^ @'Funding'@
selectVolumeFunding = selectJoin '($)
  [ fundingRow
  , joinOn "volume_funding.funder = funder.fundref_id" selectFunder
  ]
