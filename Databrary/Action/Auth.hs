module Databrary.Action.Auth
  ( AuthRequest(..)
  , AuthBAction
  , appAuth
  ) where

import Control.Monad.Has

import Databrary.Resource
import Databrary.Action.Types
import Databrary.Action.App
import Databrary.Identity

data AuthRequest = AuthRequest
  { authApp :: !AppRequest
  , authIdentity :: Identity
  }

instance Has Resource AuthRequest where
  had = had . authApp

instance Has Request AuthRequest where
  had = had . authApp

instance Has Identity AuthRequest where
  had = authIdentity

type AuthT = ActionT AuthRequest
type AuthM r = ActionM AuthRequest r
type AuthAction r = Action AuthRequest r
type AuthBAction = BAction AuthRequest

appAuth :: AuthM r a -> AppM r a
appAuth f = do
  i <- getIdentity
  withAction (\a -> AuthRequest a i) f
