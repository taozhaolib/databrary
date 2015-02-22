{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Request
  ( Wai.Request
  , MonadHasRequest
  , lookupRequestHeader
  , lookupRequestHeaders
  , lookupQueryParameters
  , requestHost
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Types (HeaderName)
import qualified Network.Wai as Wai

import Control.Has (MonadHas)

type MonadHasRequest c m = MonadHas Wai.Request c m

lookupRequestHeader :: HeaderName -> Wai.Request -> Maybe BS.ByteString
lookupRequestHeader h = lookup h . Wai.requestHeaders

lookupRequestHeaders :: HeaderName -> Wai.Request -> [BS.ByteString]
lookupRequestHeaders h = map snd . filter ((h ==) . fst) . Wai.requestHeaders

lookupQueryParameters :: BS.ByteString -> Wai.Request -> [Maybe BS.ByteString]
lookupQueryParameters q = map snd . filter ((q ==) . fst) . Wai.queryString

requestHost :: Wai.Request -> BS.ByteString
requestHost req =
  (if Wai.isSecure req then "https://" else "http://")
  <> fromMaybe "databrary.org" (lookupRequestHeader "host" req)
