module Databrary.Action.Request
  ( Request
  , MonadHasRequest
  , getRequestHeader
  , getRequestHeaders
  , getQueryParameters
  ) where

import qualified Data.ByteString as BS
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request, requestHeaders, queryString)

import Control.Has (MonadHas, peeks)

type MonadHasRequest c m = MonadHas Request c m

getRequestHeader :: MonadHasRequest c m => HeaderName -> m (Maybe BS.ByteString)
getRequestHeader h = peeks $ lookup h . requestHeaders

getRequestHeaders :: MonadHasRequest c m => HeaderName -> m [BS.ByteString]
getRequestHeaders h = peeks $ map snd . filter ((h ==) . fst) . requestHeaders

getQueryParameters :: MonadHasRequest c m => BS.ByteString -> m [Maybe BS.ByteString]
getQueryParameters q = peeks $ map snd . filter ((q ==) . fst) . queryString
