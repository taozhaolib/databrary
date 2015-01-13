{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, DataKinds #-}
module Databrary.Model.Types.Id 
  ( Id(..)
  , HasId(..)
  ) where

import Control.Monad (liftM)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))

newtype Id a = Id { unId :: Int32 } deriving (Eq, Ord)

instance PGParameter "integer" (Id a) where
  pgEncode t (Id i) = pgEncode t i
  pgEncodeValue e t (Id i) = pgEncodeValue e t i
  pgLiteral t (Id i) = pgLiteral t i
instance PGColumn "integer" (Id a) where
  pgDecode t i = Id (pgDecode t i)
  pgDecodeValue e t i = Id (pgDecodeValue e t i)

class HasId a where
  idOf :: a -> Id a
  kindOf :: a -> BS.ByteString
