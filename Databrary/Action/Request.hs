{-# LANGUAGE OverloadedStrings #-}
module Databrary.Action.Request
  ( Request
  , MonadHasRequest
  , getRequestHeader
  , getRequestHeaders
  , isApi
  ) where

import qualified Data.ByteString as BS
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request, requestHeaders, pathInfo)

import Control.Has (MonadHas, peeks)

type MonadHasRequest c m = MonadHas Request c m

getRequestHeader :: MonadHasRequest c m => HeaderName -> m (Maybe BS.ByteString)
getRequestHeader h = peeks $ lookup h . requestHeaders

getRequestHeaders :: MonadHasRequest c m => HeaderName -> m [BS.ByteString]
getRequestHeaders h = peeks $ map snd . filter ((h ==) . fst) . requestHeaders

isApi :: MonadHasRequest c m => m Bool
isApi = peeks (isapi . pathInfo) where
  isapi ("api":_) = True
  isapi _ = False
