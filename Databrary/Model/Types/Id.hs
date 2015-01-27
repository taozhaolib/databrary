{-# LANGUAGE ScopedTypeVariables, DataKinds #-}
module Databrary.Model.Types.Id 
  ( Id(..)
  , HasId(..)
  ) where

import Control.Applicative ((<$>))
import Data.Int (Int32)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))

import Control.Has (Has(..))
import qualified Databrary.Web.Route as R

newtype Id a = Id { unId :: Int32 } deriving (Eq, Ord)

instance PGParameter "integer" (Id a) where
  pgEncode t (Id i) = pgEncode t i
  pgEncodeValue e t (Id i) = pgEncodeValue e t i
  pgLiteral t (Id i) = pgLiteral t i
instance PGColumn "integer" (Id a) where
  pgDecode t i = Id (pgDecode t i)
  pgDecodeValue e t i = Id (pgDecodeValue e t i)

class Has (Id a) a => HasId a where
  kindOf :: a -> T.Text

instance HasId a => R.Routable (Id a) where
  route = R.fixed (kindOf (undefined :: a)) >> Id <$> R.reader T.decimal
  toRoute (Id i) = [kindOf (undefined :: a), T.pack $ show i]
