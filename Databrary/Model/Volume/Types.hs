{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Volume.Types
  ( Volume(..)
  ) where

import Data.Int (Int32)
import qualified Data.Text as T

import Control.Has (Has(..))
import Databrary.Kind
import Databrary.Model.Id.Types

type instance IdType Volume = Int32

data Volume = Volume
  { volumeId :: Id Volume
  , volumeName :: T.Text
  , volumeAlias :: Maybe T.Text
  , volumeBody :: Maybe T.Text
--  , volumeCreation :: Timestamp
  }

instance Has (Id Volume) Volume where
  view f v = fmap (\i -> v{ volumeId = i }) $ f $ volumeId v
  see = volumeId
instance Kinded Volume where
  kindOf _ = "volume"
