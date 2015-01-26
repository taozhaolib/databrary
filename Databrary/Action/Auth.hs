{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.Auth
  ( AuthRequest(..)
  , AuthM
  , AuthAction
  , AuthBAction
  , appAuth
  ) where

import Control.Has (makeHasFor)
import Databrary.Action.Types
import Databrary.Action.App
import Databrary.Model.Types.Party
import Databrary.Model.Authorize
import Databrary.Identity
import Databrary.Resource
import Databrary.Types.Time

data AuthRequest = AuthRequest
  { authApp :: !AppRequest
  , authIdentity :: !Identity
  }

makeHasFor ''AuthRequest
  [ ('authApp, [''Resource, ''Request, ''Timestamp])
  , ('authIdentity, [''PartyAuth, ''Authorization, ''Party, ''Access])
  ]

type AuthM r = ActionM AuthRequest r
type AuthAction r = Action AuthRequest r
type AuthBAction = BAction AuthRequest

appAuth :: AuthM r a -> AppM r a
appAuth f = do
  i <- getIdentity
  withAction (\a -> AuthRequest a i) f
