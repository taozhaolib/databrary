{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Model.Citation.Types
  ( Citation(..)
  ) where

import Control.Applicative ((<$>))
import Data.Int (Int16)
import Data.Maybe (catMaybes)
import qualified Data.Text as T

import qualified Databrary.JSON as JSON
import Databrary.URL (URI)

data Citation = Citation
  { citationHead :: T.Text
  , citationTitle :: Maybe T.Text
  , citationURL :: Maybe URI
  , citationYear :: Maybe Int16
  }

instance JSON.ToJSON Citation where
  toJSON Citation{..} = JSON.Object $ JSON.object $ catMaybes
    [ Just $ "head" JSON..= citationHead
    , ("title" JSON..=) <$> citationTitle
    , ("url" JSON..=) <$> citationURL
    , ("year" JSON..=) <$> citationYear
    ]
