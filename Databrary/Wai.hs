module Databrary.Wai
  ( WaiT
  , runWaiT
  , WaiResult
  , WaiApplication
  , ResponseHeaders
  , Status
  ) where

import Blaze.ByteString.Builder (Builder)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (RWST(..))
import Data.ByteString.Lazy (ByteString)
import Data.Monoid (mempty)
import qualified Network.Wai as Wai
import Network.HTTP.Types (ResponseHeaders, Status)

type WaiT = RWST Wai.Request ResponseHeaders
type WaiM r = WaiT r IO

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

runWaiT :: (MonadIO m, WaiResult r) => WaiT r m Status -> WaiApplication m
runWaiT (RWST wt) request send = do
  (s, r, h) <- wt request resultEmpty
  liftIO $ send $ resultResponse s h r
