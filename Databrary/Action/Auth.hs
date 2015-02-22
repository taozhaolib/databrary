{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.Auth
  ( AuthRequest(..)
  , MonadHasAuthRequest
  , AuthAction
  , withAuth
  , withoutAuth
  ) where

import Control.Monad.Reader (ReaderT, withReaderT, asks)

import Control.Has (makeHasRec)
import Databrary.Action.Types
import Databrary.Action.App
import Databrary.Model.Identity

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
  i <- determineIdentity
  withReaderT (\a -> AuthRequest a i) f

withoutAuth :: ReaderT AuthRequest IO a -> ReaderT AppRequest IO a
withoutAuth f =
  withReaderT (\a -> AuthRequest a UnIdentified) f
