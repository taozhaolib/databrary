{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables, DataKinds #-}
module Databrary.Model.Id where

import Control.Monad (liftM)
import qualified Data.ByteString as BS
import Data.Int (Int32)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..), PGBinaryParameter(..), PGBinaryColumn(..))
import Snap.Core (MonadSnap, dir, pathArg)
import Snap.Util.Readable (Readable(..))

newtype Id a = Id { unId :: Int32 } deriving (Eq, Ord)

instance PGParameter "integer" (Id a) where
  pgEncode t (Id i) = pgEncode t i
  pgLiteral t (Id i) = pgLiteral t i
instance PGBinaryParameter "integer" Int32 => PGBinaryParameter "integer" (Id a) where
  pgEncodeBinary e t (Id i) = pgEncodeBinary e t i
instance PGColumn "integer" (Id a) where
  pgDecode t i = Id (pgDecode t i)
instance PGBinaryColumn "integer" Int32 => PGBinaryColumn "integer" (Id a) where
  pgDecodeBinary e t i = Id (pgDecodeBinary e t i)

instance Readable (Id a) where
  fromBS = liftM (Id . fromIntegral :: Int -> Id a) . fromBS

class HasId a where
  key :: a -> Id a
  kind :: a -> BS.ByteString

pathIdArg :: forall a b m . (MonadSnap m, HasId a) => (Id a -> m b) -> m b
pathIdArg f = dir (kind (undefined :: a)) $ pathArg f
