{-# LANGUAGE TemplateHaskell, TypeFamilies, OverloadedStrings #-}
module Databrary.Model.RecordCategory.Types
  ( RecordCategory(..)
  , MonadHasRecordCategory
  ) where

import Data.Int (Int16)
import qualified Data.Text as T
import Language.Haskell.TH.Lift (deriveLift)

import Control.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Metric.Types
import Databrary.Model.Id.Types

type instance IdType RecordCategory = Int16

data RecordCategory = RecordCategory
  { recordCategoryId :: Id RecordCategory
  , recordCategoryName :: T.Text
  , recordCategoryTemplate :: [(Metric, Bool)]
  }

instance Kinded RecordCategory where
  kindOf _ = "category"

makeHasRec ''RecordCategory ['recordCategoryId]
deriveLift ''RecordCategory
