{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.Auth
  ( AuthRequest(..)
  , AuthAction
  , AuthM
  , runAuth
  ) where

import Control.Has (HasM, makeHasFor)
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

type AuthM c m = HasM AuthRequest c m

runAuth :: AuthAction -> AppAction
runAuth f = do
  i <- getIdentity
  withAction (\a -> AuthRequest a i) f
