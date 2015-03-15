{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Funding.Types
  ( Funder(..)
  , MonadHasFunder
  , Funding(..)
  , MonadHasFunding
  ) where

import Data.Int (Int64)
import qualified Data.Text as T

import Databrary.Has (makeHasRec)
import qualified Databrary.JSON as JSON
import Databrary.Model.Id.Types

type instance IdType Funder = Int64

data Funder = Funder
  { funderId :: Id Funder
  , funderName :: T.Text
  }

makeHasRec ''Funder ['funderId]

instance JSON.ToJSON Funder where
  toJSON Funder{..} = JSON.Object $ JSON.object
    [ "id" JSON..= funderId
    , "name" JSON..= funderName
    ]

data Funding = Funding
  { fundingFunder :: Funder
  , fundingAwards :: [T.Text]
  }

makeHasRec ''Funding ['fundingFunder]

instance JSON.ToJSON Funding where
  toJSON Funding{..} = JSON.Object $ JSON.object
    [ "funder" JSON..= fundingFunder
    , "awards" JSON..= fundingAwards
    ]

