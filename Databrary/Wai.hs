module Databrary.Wai
  ( WaiT
  , runWaiT
  , WaiResult
  , WaiApplication
  , Wai.Request
  , ResponseHeaders
  , Status
  , routeWai
  ) where

import Blaze.ByteString.Builder (Builder)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (RWST(..))
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mempty)
import qualified Network.Wai as Wai
import Network.HTTP.Types (ResponseHeaders, Status)

import Databrary.Route (RouteM, routeRequest)

type WaiT = RWST Wai.Request ResponseHeaders
type WaiM r = WaiT r IO
type WaiR r m = WaiT r m Status
type Wai r = WaiT r IO Status

class WaiResult r where
  resultEmpty :: r
  resultResponse :: Status -> ResponseHeaders -> r -> Wai.Response

instance WaiResult Builder where
  resultEmpty = mempty
  resultResponse = Wai.responseBuilder

instance WaiResult ByteString where
  resultEmpty = mempty
  resultResponse = Wai.responseLBS

type WaiApplication m = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> m Wai.ResponseReceived

runWaiT :: (MonadIO m, WaiResult r) => WaiR r m -> WaiApplication m
runWaiT (RWST wai) request send = do
  (s, r, h) <- wai request resultEmpty
  liftIO $ send $ resultResponse s h r

routeWai :: (MonadIO m, WaiResult r) => RouteM (WaiR r m) -> WaiApplication m
routeWai route request = runWaiT wai request
  where Just wai = routeRequest route request
