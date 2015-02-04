{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.Auth
  ( AuthRequest(..)
  , AuthAction
  , withAuth
  ) where

import Control.Monad.Reader (asks)

import Control.Has (makeHasFor)
import Databrary.Action.Types
import Databrary.Action.Request
import Databrary.Action.App
import Databrary.Model.Party.Types
import Databrary.Model.Authorize
import Databrary.Identity
import Databrary.Resource
import Databrary.Time

data AuthRequest = AuthRequest
  { authApp :: !AppRequest
  , authIdentity :: !Identity
  }

makeHasFor ''AuthRequest
  [ ('authApp, [''Resource, ''Request, ''Timestamp])
  , ('authIdentity, [''PartyAuth, ''Authorization, ''Party, ''Access])
  ]

type AuthAction = Action AuthRequest

instance ActionData AuthRequest where
  returnResponse s h r = asks (returnResponse s h r . authApp)

withAuth :: AuthAction -> AppAction
withAuth f = do
  i <- getIdentity
  withAction (\a -> AuthRequest a i) f
