module Databrary.Action.Auth
  ( AuthRequest(..)
  , AuthBAction
  , appAuth
  ) where

import Databrary.Resource
import Databrary.Action.Types
import Databrary.Action.App
import Databrary.Identity

data AuthRequest = AuthRequest
  { authApp :: !AppRequest
  , authIdentity :: Identity
  }

instance HasResource AuthRequest where
  toResource = toResource . authApp

instance HasRequest AuthRequest where
  toRequest = toRequest . authApp

type AuthT = ActionT AuthRequest
type AuthM r = ActionM AuthRequest r
type AuthAction r = Action AuthRequest r
type AuthBAction = BAction AuthRequest

appAuth :: AuthM r a -> AppM r a
appAuth f = do
  i <- getIdentity
  withAction (\a -> AuthRequest a i) f
