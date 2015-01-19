{-# LANGUAGE UndecidableInstances #-}
module Databrary.Action.Auth
  ( AuthRequest(..)
  , AuthT
  , AuthM
  , AuthAction
  , AuthBAction
  , appAuth
  ) where

import Control.Monad.Has

import Databrary.Action.Types
import Databrary.Action.App
import Databrary.Identity

data AuthRequest = AuthRequest
  { authApp :: !AppRequest
  , authIdentity :: !Identity
  }

instance Has a AppRequest => Has a AuthRequest where
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
