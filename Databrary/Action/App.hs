module Databrary.Action.App 
  ( AppRequest(..)
  , AppT
  , AppM
  , AppAction
  , AppBAction
  ) where

import qualified Network.Wai as Wai

import Control.Monad.Has
import Databrary.Resource
import Databrary.Types.Time
import Databrary.Action.Types

data AppRequest = AppRequest
  { appResource :: !Resource
  , appRequest :: !Wai.Request
  , appTimestamp :: !Timestamp
  }

instance Has Resource AppRequest where
  had = appResource

instance Has Wai.Request AppRequest where
  had = appRequest

instance Has Timestamp AppRequest where
  had = appTimestamp

type AppT = ActionT AppRequest
type AppM r = ActionM AppRequest r
type AppAction r = Action AppRequest r
type AppBAction = BAction AppRequest
