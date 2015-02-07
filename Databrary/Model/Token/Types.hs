{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Token.Types
  ( TokenId
  , Token(..)
  , AccountToken(..)
  , MonadHasAccountToken
  , SessionToken(..)
  , MonadHasSessionToken
  ) where

import qualified Data.ByteString as BS

import Control.Has (makeHasRec)
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

makeHasRec ''AccountToken ['accountToken, 'tokenAccount]

data SessionToken = SessionToken
  { sessionAccountToken :: !AccountToken
  , sessionSuperuser :: Bool
  , sessionAccess :: Access
  }

makeHasRec ''SessionToken ['sessionAccountToken, 'sessionAccess]
