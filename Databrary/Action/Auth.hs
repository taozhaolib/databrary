{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.Auth
  ( AuthRequest(..)
  , MonadHasAuthRequest
  , AuthAction
  , withAuth
  ) where

import Control.Monad.Reader (asks)

import Control.Has (makeHasRec)
import Databrary.Action.Types
import Databrary.Action.App
import Databrary.Identity

data AuthRequest = AuthRequest
  { authApp :: !AppRequest
  , authIdentity :: !Identity
  }

makeHasRec ''AuthRequest ['authApp, 'authIdentity]

type AuthAction = Action AuthRequest

instance ActionData AuthRequest where
  returnResponse s h r = asks (returnResponse s h r . authApp)

withAuth :: AuthAction -> AppAction
withAuth f = do
  i <- getIdentity
  withAction (\a -> AuthRequest a i) f
