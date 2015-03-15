{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, DataKinds #-}
module Databrary.Model.Tag.Types
  ( TagName(..)
  , Tag(..)
  , MonadHasTag
  , TagUse(..)
  , MonadHasTagUse
  ) where

import qualified Data.ByteString as BS
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))

import Databrary.Has (makeHasRec)
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Slot.Types

newtype TagName = TagName BS.ByteString

instance PGParameter "character varying" TagName where
  pgEncode t (TagName n) = pgEncode t n
  pgEncodeValue e t (TagName n) = pgEncodeValue e t n
  pgLiteral t (TagName n) = pgLiteral t n
instance PGColumn "character varying" TagName where
  pgDecode t = TagName . pgDecode t
  pgDecodeValue e t = TagName . pgDecodeValue e t

type instance IdType Tag = Int32

data Tag = Tag
  { tagId :: Id Tag
  , tagName :: TagName
  }

data TagUse = TagUse
  { tag :: Tag
  , tagKeyword :: Bool
  , tagWho :: Account
  , tagSlot :: Slot
  }

instance Kinded Tag where
  kindOf _ = "tag"

makeHasRec ''Tag ['tagId, 'tagName]
makeHasRec ''TagUse ['tag, 'tagWho, 'tagSlot]
