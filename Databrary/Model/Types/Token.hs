module Databrary.Model.Types.Token
  ( TokenId
  , Token(..)
  , AccountToken(..)
  , SessionToken(..)
  , sessionAccount
  ) where

import qualified Data.ByteString as BS

import Databrary.Model.Types.Party
import Databrary.Model.Types.Authorize

type TokenId = BS.ByteString

data Token = Token
  { tokenId :: TokenId
  , tokenExpires :: UTCTime
  }

data AccountToken = AccountToken
  { accountToken :: !Token
  , tokenAccount :: Account
  }

data SessionToken = SessionToken
  { sessionToken :: !AccountToken
  , sessionAccess :: Access
  }

sessionAccount :: SessionToken -> Account
sessionAccount = tokenAccount . sessionToken
