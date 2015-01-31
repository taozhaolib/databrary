{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action
  ( Request
  , Action
  , ActionM
  , getRequestHeader
  , AppAction
  , AuthAction

  , Response
  , returnResponse
  , notFoundResponse
  , okResponse
  , result

  , StdMethod(GET, POST)
  , toRoute
  , apiRoute
  , AppRAction
  , AuthRAction
  , action
  , actionMethod
  , actionRoute
  ) where

import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(..))

import Databrary.Action.Types
import Databrary.Action.Request
import Databrary.Action.Response
import Databrary.Action.App
import Databrary.Action.Auth
import Databrary.Action.Route
import Databrary.Web.Route (toRoute)

apiRoute :: Bool -> [T.Text] -> [T.Text]
apiRoute False = id
apiRoute True = ("api" :)

type AppRAction = RouteAction AppRequest
type AuthRAction = RouteAction AuthRequest
