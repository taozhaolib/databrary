module Databrary.Model.Types.Token
  ( TokenId
  , Token(..)
  , AccountToken(..)
  , SessionToken(..)
  , sessionAccount
  ) where

import qualified Data.ByteString as BS

import Databrary.Types.Time
import Databrary.Model.Types.Party
import Databrary.Model.Types.Authorize

type TokenId = BS.ByteString

data Token = Token
  { tokenId :: TokenId
  , tokenExpires :: Timestamp
  }

data AccountToken = AccountToken
  { accountToken :: !Token
  , tokenAccount :: Account
  }

data SessionToken = SessionToken
  { sessionToken :: !AccountToken
  , sessionSuperuser :: Maybe Timestamp
  , sessionAccess :: Access
  }

sessionAccount :: SessionToken -> Account
sessionAccount = tokenAccount . sessionToken
