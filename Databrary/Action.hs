module Databrary.Action
  ( Request
  , RequestM

  , getRequestHeader
  , responseHeader
  , respond
  , okResult
  , notFoundResult
  , jsonResult
  , resultWith

  , ActionM
  , Action
  , AppBAction
  , AuthBAction

  , bAction
  , StdMethod(GET, POST)
  , toRoute
  , AppRAction
  , AuthRAction
  ) where

import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(..))

import Databrary.Action.Types
import Databrary.Action.Response
import Databrary.Action.App
import Databrary.Action.Auth
import Databrary.Action.Route

bAction :: StdMethod -> [T.Text] -> BAction q -> RouteAction q
bAction = action

type AppRAction = RouteAction AppRequest
type AuthRAction = RouteAction AuthRequest
