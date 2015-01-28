{-# LANGUAGE FlexibleContexts, ConstraintKinds, OverloadedStrings #-}
module Databrary.Action.Request
  ( RequestM
  , Request
  , getRequestHeader
  , isApi
  ) where

import qualified Data.ByteString as BS
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request, requestHeaders, pathInfo)

import Control.Has (HasM, peeks)

type RequestM c m = HasM Request c m

getRequestHeader :: RequestM c m => HeaderName -> m (Maybe BS.ByteString)
getRequestHeader h = peeks (lookup h . requestHeaders)

isApi :: RequestM c m => m Bool
isApi = peeks (isapi . pathInfo) where
  isapi ("api":_) = True
  isapi _ = False
