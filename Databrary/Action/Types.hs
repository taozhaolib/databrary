{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Databrary.Action.Types
  ( RequestM
  , Request
  , getRequestHeader
  , ResponseHeaderM
  , ActionT
  , ActionM
  , Action
  , BAction
  , withAction
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import Control.Monad.RWS.Strict (RWST, withRWST)
import Control.Monad.Writer.Class (MonadWriter)
import qualified Data.ByteString as BS
import Network.HTTP.Types (ResponseHeaders, HeaderName, Status)
import Network.Wai (Request, requestHeaders)

import Control.Has (HasM, peeks)

type RequestM c m = HasM Request c m
type ResponseHeaderM m = MonadWriter ResponseHeaders m

getRequestHeader :: RequestM c m => HeaderName -> m (Maybe BS.ByteString)
getRequestHeader h = peeks (lookup h . requestHeaders)

type ActionT q = RWST q ResponseHeaders
type ActionM q r = ActionT q r IO
type Action q r = ActionM q r Status
type BAction q = Action q Blaze.Builder

withAction :: (q -> q') -> ActionT q' r m a -> ActionT q r m a
withAction f = withRWST $ (,) . f
