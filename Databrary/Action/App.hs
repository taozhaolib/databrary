{-# LANGUAGE TemplateHaskell #-}
module Databrary.Action.App 
  ( AppRequest(..)
  , AppM
  , AppAction
  , AppBAction
  ) where

import Control.Has
import Databrary.Resource
import Databrary.Types.Time
import Databrary.Action.Types
import Databrary.Action.Request

data AppRequest = AppRequest
  { appResource :: !Resource
  , appRequest :: !Request
  , appTimestamp :: !Timestamp
  }

makeHasFor ''AppRequest
  [ ('appResource, [])
  , ('appRequest, [])
  , ('appTimestamp, [])
  ]

type AppM r = ActionM AppRequest r
type AppAction r = Action AppRequest r
type AppBAction = BAction AppRequest
