{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Databrary.Model.Inet
  ( Inet(..)
  ) where

import qualified Data.ByteString as BS
import Database.PostgreSQL.Typed.Types (PGType, PGParameter(..), PGColumn(..))

newtype Inet = Inet { inetAddr :: BS.ByteString }

instance PGType "inet"
instance PGParameter "inet" Inet where
  pgEncode _ = inetAddr
instance PGColumn "inet" Inet where
  pgDecode _ = Inet
