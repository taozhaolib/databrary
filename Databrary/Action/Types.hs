{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Databrary.Action.Types
  ( RequestM
  , Request
  , getRequest
  , getRequestHeader
  , ActionT
  , ActionM
  , Action
  , BAction
  , withAction
  , notFound
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import Control.Monad (liftM)
import Control.Monad.RWS.Strict (RWST, withRWST)
import qualified Data.ByteString as BS
import Network.HTTP.Types (ResponseHeaders, HeaderName, Status, notFound404)
import Network.Wai (Request, requestHeaders)

import Control.Monad.Has

type RequestM = HasM Request

getRequest :: RequestM m => m Request
getRequest = pull

getRequestHeader :: RequestM m => HeaderName -> m (Maybe BS.ByteString)
getRequestHeader h = liftM (lookup h . requestHeaders) getRequest

type ActionT q = RWST q ResponseHeaders
type ActionM q r = ActionT q r IO
type Action q r = ActionM q r Status
type BAction q = Action q Blaze.Builder

withAction :: (q -> q') -> ActionT q' r m a -> ActionT q r m a
withAction f = withRWST $ (,) . f

notFound :: BAction q
notFound = return notFound404
