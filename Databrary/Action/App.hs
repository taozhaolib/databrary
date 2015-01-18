module Databrary.Action.App 
  ( AppRequest(..)
  , AppT
  , AppM
  , AppAction
  , AppBAction
  , initApp
  ) where

import qualified Network.Wai as Wai

import Control.Monad.Has
import Databrary.Resource
import Databrary.Action.Types

data AppRequest = AppRequest
  { appResource :: !Resource
  , appRequest :: !Wai.Request
  }

instance Has Resource AppRequest where
  had = appResource

instance Has Wai.Request AppRequest where
  had = appRequest

type AppT = ActionT AppRequest
type AppM r = ActionM AppRequest r
type AppAction r = Action AppRequest r
type AppBAction = BAction AppRequest

initApp :: Resource -> Wai.Request -> AppRequest
initApp rc rq = AppRequest
  { appResource = rc
  , appRequest = rq
  }
