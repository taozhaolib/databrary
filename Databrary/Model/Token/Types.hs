{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Databrary.Model.Token.Types
  ( Token(..)
  , AccountToken(..)
  , MonadHasAccountToken
  , SessionToken(..)
  , MonadHasSessionToken
  , UploadToken(..)
  , MonadHasUploadToken
  ) where

import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Has (makeHasRec)
import Databrary.Model.Time.Types
import Databrary.Model.Id.Types
import Databrary.Model.Party.Types

type instance IdType Token = BS.ByteString

data Token = Token
  { tokenId :: Id Token
  , tokenExpires :: Timestamp
  }

data AccountToken = AccountToken
  { accountToken :: !Token
  , tokenAccount :: SiteAuth
  }

makeHasRec ''AccountToken ['accountToken, 'tokenAccount]

data SessionToken = SessionToken
  { sessionAccountToken :: !AccountToken
  , sessionSuperuser :: Bool
  }

makeHasRec ''SessionToken ['sessionAccountToken]

data UploadToken = UploadToken
  { uploadAccountToken :: AccountToken
  , uploadFilename :: T.Text
  }

makeHasRec ''UploadToken ['uploadAccountToken]
