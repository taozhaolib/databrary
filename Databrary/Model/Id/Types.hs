{-# LANGUAGE ScopedTypeVariables, DataKinds, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module Databrary.Model.Id.Types 
  ( IdType
  , Id(..)
  , HasId(..)
  ) where

import Control.Applicative ((<$>))
import qualified Data.Text as T
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Control.Has (Has(..))
import qualified Databrary.Web.Route as R

type family IdType a
newtype Id a = Id { unId :: IdType a }

instance Eq (IdType a) => Eq (Id a) where
  Id a == Id b = a == b
  Id a /= Id b = a /= b

instance Ord (IdType a) => Ord (Id a) where
  Id a `compare` Id b = a `compare` b
  Id a < Id b = a < b
  Id a <= Id b = a <= b
  Id a >= Id b = a >= b
  Id a > Id b = a > b
  Id a `min` Id b = Id $ a `min` b
  Id a `max` Id b = Id $ a `max` b

instance PGParameter t (IdType a) => PGParameter t (Id a) where
  pgEncode t (Id i) = pgEncode t i
  pgEncodeValue e t (Id i) = pgEncodeValue e t i
  pgLiteral t (Id i) = pgLiteral t i
instance PGColumn t (IdType a) => PGColumn t (Id a) where
  pgDecode t i = Id (pgDecode t i)
  pgDecodeValue e t i = Id (pgDecodeValue e t i)

class Has (Id a) a => HasId a where
  kindOf :: a -> T.Text

instance (R.Routable (IdType a), HasId a) => R.Routable (Id a) where
  route = R.fixed (kindOf (undefined :: a)) >> Id <$> R.route
  toRoute (Id i) = kindOf (undefined :: a) : R.toRoute i

instance TH.Lift (IdType a) => TH.Lift (Id a) where
  lift (Id i) = TH.conE 'Id `TH.appE` TH.lift i
