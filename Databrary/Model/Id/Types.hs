{-# LANGUAGE DataKinds, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
module Databrary.Model.Id.Types 
  ( IdType
  , Id(..)
  , Int32
  ) where

import Control.Applicative ((<$>))
import qualified Data.Aeson as JSON
import Data.Int (Int32)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import Database.PostgreSQL.Typed.Dynamic (PGRep)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import Control.Has (Has(..))
import Databrary.Kind
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
instance (PGParameter t (IdType a), PGColumn t (IdType a), PGRep t (IdType a)) => PGRep t (Id a)

instance (R.Routable (IdType a), Has (Id a) a, Kinded a) => R.Routable (Id a) where
  route = R.fixed (kindOf (undefined :: a)) >> Id <$> R.route
  toRoute (Id i) = kindOf (undefined :: a) : R.toRoute i

instance JSON.ToJSON (IdType a) => JSON.ToJSON (Id a) where
  toJSON (Id a) = JSON.toJSON a
instance JSON.FromJSON (IdType a) => JSON.FromJSON (Id a) where
  parseJSON = fmap Id . JSON.parseJSON

instance TH.Lift (IdType a) => TH.Lift (Id a) where
  lift (Id i) = TH.conE 'Id `TH.appE` TH.lift i
