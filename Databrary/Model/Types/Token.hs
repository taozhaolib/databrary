module Databrary.Model.Types.Token
  ( TokenId
  , Token(..)
  ) where

import qualified Data.ByteString as BS

type TokenId = BS.ByteString

data Token = Token
  { tokenId :: TokenId
  , tokenExpires :: UTCTime
  }
