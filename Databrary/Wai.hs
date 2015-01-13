{-# LANGUAGE FlexibleInstances #-}
module Databrary.Wai
  ( WaiT
  , runWaiT
  , Result
  , WaiApplication
  , Wai.Request
  , ResponseHeaders
  , Status
  ) where

import Blaze.ByteString.Builder (Builder)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS (RWST(..))
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid (mempty)
import qualified Network.Wai as Wai
import Network.HTTP.Types (ResponseHeaders, Status)

type WaiT = RWST Wai.Request ResponseHeaders

class Result r where
  resultEmpty :: r
  resultResponse :: Status -> ResponseHeaders -> r -> Wai.Response

instance Result (Status -> ResponseHeaders -> Wai.Response) where
  resultEmpty s h = Wai.responseBuilder s h mempty
  resultResponse s h r = r s h

instance Result Builder where
  resultEmpty = mempty
  resultResponse = Wai.responseBuilder

instance Result BSL.ByteString where
  resultEmpty = mempty
  resultResponse = Wai.responseLBS

instance Result Wai.StreamingBody where
  resultEmpty _ _ = return ()
  resultResponse = Wai.responseStream

instance Result FilePath where
  resultEmpty = "/dev/null"
  resultResponse s h f = Wai.responseFile s h f Nothing

instance Result (FilePath, Wai.FilePart) where
  resultEmpty = ("/dev/null", Wai.FilePart 0 0 0)
  resultResponse s h (f, p) = Wai.responseFile s h f (Just p)

type WaiApplication m = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> m Wai.ResponseReceived

runWaiT :: (MonadIO m, Result r) => WaiT r m Status -> WaiApplication m
runWaiT (RWST wai) request send = do
  (s, r, h) <- wai request resultEmpty
  liftIO $ send $ resultResponse s h r
