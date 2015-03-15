{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Tag.Types
  ( TagName(..)
  , Tag(..)
  , MonadHasTag
  ) where

import qualified Data.ByteString as BS

import Control.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types

newtype TagName = TagName BS.ByteString

type instance IdType Tag = Int32

data Tag = Tag
  { tagId :: Id Tag
  , tagName :: TagName
  }

instance Kinded Tag where
  kindOf _ = "tag"

makeHasRec ''Tag ['tagId, 'tagName]
