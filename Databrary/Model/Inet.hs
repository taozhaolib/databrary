{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DataKinds #-}
module Databrary.Model.Inet
  ( Inet(..)
  ) where

import qualified Data.ByteString as BS
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))

newtype Inet = Inet { inetAddr :: BS.ByteString }

instance PGParameter "inet" Inet where
  pgEncode _ = inetAddr
instance PGColumn "inet" Inet where
  pgDecode _ = Inet
