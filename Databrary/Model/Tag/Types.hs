{-# LANGUAGE OverloadedStrings, TemplateHaskell, TypeFamilies, DataKinds #-}
module Databrary.Model.Tag.Types
  ( TagName(..)
  , validateTag
  , Tag(..)
  , MonadHasTag
  , TagUse(..)
  , MonadHasTagUse
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import qualified Text.Regex.Posix as Regex

import Databrary.Ops
import Databrary.Has (makeHasRec)
import qualified Databrary.Web.Route as R
import Databrary.Model.Kind
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types
import Databrary.Model.Slot.Types

newtype TagName = TagName BS.ByteString

validTag :: Regex.Regex
validTag = Regex.makeRegex
  ("^[a-z][-a-z ]+[a-z]$" :: BS.ByteString)

validateTag :: BS.ByteString -> Maybe TagName
validateTag t = Regex.matchTest validTag tt ?> TagName tt where
  tt = BSC.unwords $ BSC.words t

instance R.Routable TagName where
  route = R.maybe . validateTag =<< R.route
  toRoute (TagName n) = R.toRoute n

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

instance Kinded Tag where
  kindOf _ = "tag"

data TagUse = TagUse
  { tag :: Tag
  , tagKeyword :: Bool
  , tagWho :: Account
  , tagSlot :: Slot
  }

makeHasRec ''Tag ['tagId, 'tagName]
makeHasRec ''TagUse ['tag, 'tagWho, 'tagSlot]
