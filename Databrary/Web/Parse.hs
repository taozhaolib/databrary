{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Databrary.Web.Parse
  ( Content(..)
  , parseRequestContent
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import Control.Applicative ((<$>))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Monoid (mempty)
import Data.Word (Word64)
import Network.HTTP.Types (requestEntityTooLarge413, hContentType)
import Network.Wai
import Network.Wai.Parse

import Control.Has (peek, peeks)
import Databrary.Action
import Databrary.Action.Response (response)

requestTooLarge :: Response
requestTooLarge = response requestEntityTooLarge413 [] (mempty :: Blaze.Builder)

type ChunkParser a = IO BS.ByteString -> IO a

_mapChunks :: (a -> b) -> ChunkParser a -> ChunkParser b
_mapChunks f parse next = f <$> parse next

nullChunks :: ChunkParser Word64
nullChunks next = go 0 where
  go n = do
    b <- next
    if BS.null b
      then return n
      else go (n + fromIntegral (BS.length b))

limitChunks :: Word64 -> ChunkParser a -> ChunkParser a
limitChunks lim parse next = do
  len <- newIORef 0
  parse $ do
    n <- readIORef len
    b <- next
    let n' = n + fromIntegral (BS.length b)
    when (n' > lim) $ result requestTooLarge
    writeIORef len n'
    return b

parserChunks :: AP.Parser a -> ChunkParser (AP.Result a)
parserChunks parser next = run (AP.parse parser) where
  run p = do
    b <- next
    let r = p b
    if BS.null b
      then return r
      else run $ AP.feed r


_mapBackEnd :: (a -> b) -> BackEnd a -> BackEnd b
_mapBackEnd f back param info next = f <$> back param info next

nullBackEnd :: BackEnd Word64
nullBackEnd _ _ = nullChunks

_limitBackEnd :: Word64 -> BackEnd a -> BackEnd a
_limitBackEnd lim back param info = limitChunks lim $ back param info

_parserBackEnd :: AP.Parser a -> BackEnd (AP.Result a)
_parserBackEnd parser _ _ = parserChunks parser


_parseRequestChunks :: (MonadIO m, ActionM c m) => ChunkParser a -> m a
_parseRequestChunks p = liftIO . p =<< peeks requestBody

limitRequestChunks :: (MonadIO m, ActionM c m) => Word64 -> ChunkParser a -> m a
limitRequestChunks lim p = do
  rq <- peek
  liftIO $ case requestBodyLength rq of
    KnownLength l | l > lim -> result requestTooLarge
    _ -> limitChunks lim p $ requestBody rq

data Content
  = ContentForm 
    { contentFormParams :: [Param]
    , contentFormFiles :: [File Word64]
    }
  | ContentJSON JSON.Value
  | ContentUnknown

maxTextSize :: Word64
maxTextSize = 1024*1024

parseFormContent :: (MonadIO m, ActionM c m) => RequestBodyType -> m Content
parseFormContent t = uncurry ContentForm
  <$> limitRequestChunks maxTextSize (sinkRequestBody nullBackEnd t)

parseJSONContent :: (MonadIO m, ActionM c m) => m Content
parseJSONContent = maybe ContentUnknown ContentJSON . AP.maybeResult
  <$> limitRequestChunks maxTextSize (parserChunks JSON.json)

parseRequestContent :: (MonadIO m, ActionM c m) => m Content
parseRequestContent = do
  ct <- getRequestHeader hContentType
  case fmap parseContentType ct of
    Just ("application/x-www-form-urlencoded", _) ->
      parseFormContent UrlEncoded
    Just ("multipart/form-data", attrs) | Just bound <- lookup "boundary" attrs ->
      parseFormContent $ Multipart bound
    Just ("text/json", _) ->
      parseJSONContent
    Just ("application/json", _) ->
      parseJSONContent
    _ -> return ContentUnknown
