{-# LANGUAGE TypeSynonymInstances #-}
module Databrary.Action.Wai
  ( WaiT
  , Result
  , respond
  , runWai
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.RWS.Strict (RWST(..))
import Control.Monad.State.Class (MonadState, modify)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Network.Wai as Wai
import Network.HTTP.Types (ResponseHeaders, Status)

import Databrary.Action.Types

type WaiT = ActionT Wai.Request

class Result r where
  resultEmpty :: r
  resultResponse :: Status -> ResponseHeaders -> r -> Wai.Response

instance Result (Status -> ResponseHeaders -> Wai.Response) where
  resultEmpty s h = Wai.responseBuilder s h mempty
  resultResponse s h r = r s h

instance Result Blaze.Builder where
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

class Result r => Respond r b where
  respond :: MonadState r m => b -> m ()

instance (Result r, Monoid r) => Respond r r where respond = modify . mappend
instance Respond Blaze.Builder T.Text where respond = respond . Blaze.fromText
instance Respond Blaze.Builder TL.Text where respond = respond . Blaze.fromLazyText
instance Respond Blaze.Builder BS.ByteString where respond = respond . Blaze.fromByteString
instance Respond Blaze.Builder BSL.ByteString where respond = respond . Blaze.fromLazyByteString

type ApplicationT m = Wai.Request -> (Wai.Response -> IO Wai.ResponseReceived) -> m Wai.ResponseReceived

runWai :: (MonadIO m, Result r) => WaiT r m Status -> ApplicationT m
runWai (RWST wai) request send = do
  (s, r, h) <- wai request resultEmpty
  liftIO $ send $ resultResponse s h r
