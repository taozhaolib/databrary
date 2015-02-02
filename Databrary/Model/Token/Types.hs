{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Token.Types
  ( TokenId
  , Token(..)
  , AccountToken(..)
  , SessionToken(..)
  ) where

import qualified Data.ByteString as BS

import Control.Has (makeHasFor)
import Databrary.Time
import Databrary.Model.Party.Types
import Databrary.Model.Authorize.Types

type TokenId = BS.ByteString

data Token = Token
  { tokenId :: TokenId
  , tokenExpires :: Timestamp
  }

data AccountToken = AccountToken
  { accountToken :: !Token
  , tokenAccount :: Account
  }

makeHasFor ''AccountToken
  [ ('accountToken, [])
  , ('tokenAccount, [''Party])
  ]

data SessionToken = SessionToken
  { sessionAccountToken :: !AccountToken
  , sessionSuperuser :: Bool
  , sessionAccess :: Access
  }

makeHasFor ''SessionToken
  [ ('sessionAccountToken, [''Token, ''Account, ''Party])
  , ('sessionAccess, [])
  ]
