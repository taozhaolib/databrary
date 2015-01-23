{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.Auth
  ( AuthRequest(..)
  , AuthT
  , AuthM
  , AuthAction
  , AuthBAction
  , appAuth
  ) where

import qualified Network.Wai as Wai

import Control.Has (makeHasFor)
import Databrary.Action.Types
import Databrary.Action.App
import Databrary.Model.Authorize
import Databrary.Identity
import Databrary.Resource
import Databrary.Types.Time

data AuthRequest = AuthRequest
  { authApp :: !AppRequest
  , authIdentity :: !Identity
  }

makeHasFor 
  [ ('authApp, [''Resource, ''Wai.Request, ''Timestamp])
  , ('authIdentity, [''Authorization, ''Access])
  ] ''AuthRequest

type AuthT = ActionT AuthRequest
type AuthM r = ActionM AuthRequest r
type AuthAction r = Action AuthRequest r
type AuthBAction = BAction AuthRequest

appAuth :: AuthM r a -> AppM r a
appAuth f = do
  i <- getIdentity
  withAction (\a -> AuthRequest a i) f
