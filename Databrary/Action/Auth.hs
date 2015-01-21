{-# LANGUAGE UndecidableInstances #-}
module Databrary.Action.Auth
  ( AuthRequest
  , AuthT
  , AuthM
  , AuthAction
  , AuthBAction
  , appAuth
  ) where

import Databrary.Action.Types
import Databrary.Action.App
import Databrary.Identity

type AuthRequest = Auth AppRequest

type AuthT = ActionT AuthRequest
type AuthM r = ActionM AuthRequest r
type AuthAction r = Action AuthRequest r
type AuthBAction = BAction AuthRequest

appAuth :: AuthM r a -> AppM r a
appAuth f = do
  i <- getIdentity
  withAction (\a -> Auth a i) f
