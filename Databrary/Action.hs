{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action
  ( Request
  , RequestM

  , ActionM
  , Action
  , AppBAction
  , AuthBAction
  , BResult

  , getRequestHeader
  , responseHeader
  , respond
  , notFoundResult
  , jsonResult
  , htmlResult
  , ResultM
  , BResultM
  , resultWith

  , bAction
  , StdMethod(GET, POST)
  , toRoute
  , apiRoute
  , AppRAction
  , AuthRAction
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

bAction :: StdMethod -> [T.Text] -> BAction q -> RouteAction q
bAction = action

apiRoute :: Bool -> [T.Text] -> [T.Text]
apiRoute False = id
apiRoute True = ("api" :)

type AppRAction = RouteAction AppRequest
type AuthRAction = RouteAction AuthRequest
