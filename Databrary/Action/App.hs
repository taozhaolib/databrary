{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.App 
  ( AppRequest(..)
  , AppT
  , AppM
  , AppAction
  , AppBAction
  ) where

import qualified Network.Wai as Wai

import Control.Has
import Databrary.Resource
import Databrary.Types.Time
import Databrary.Action.Types

data AppRequest = AppRequest
  { appResource :: !Resource
  , appRequest :: !Wai.Request
  , appTimestamp :: !Timestamp
  }

makeHasFor 
  [ ('appResource, [])
  , ('appRequest, [])
  , ('appTimestamp, [])
  ] ''AppRequest

type AppT = ActionT AppRequest
type AppM r = ActionM AppRequest r
type AppAction r = Action AppRequest r
type AppBAction = BAction AppRequest
