{-# LANGUAGE OverloadedStrings, PatternGuards, ConstraintKinds #-}
module Databrary.Web.Parse
  ( Content(..)
  , parseRequestContent
  ) where

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import Network.Wai
import Network.Wai.Parse

import Control.Has (peek, peeks)
import Databrary.Action.Types

type ChunkParser a = IO BS.ByteString -> IO a

mapChunks :: (a -> b) -> ChunkParser a -> ChunkParser b
mapChunks f parse next = f <$> parse next

nullChunks :: ChunkParser Word64
nullChunks next = go 0 where
  go n = do
    b <- next
    if BS.null b
      then return n
      else go (n + fromIntegral (BS.length b))

limitChunks :: Word64 -> ChunkParser a -> ChunkParser (Either Word64 a)
limitChunks lim parse next = do
  len <- newIORef 0
  let go n = do
        b <- next
        let n' = n + fromIntegral (BS.length b)
        if BS.null b || n' <= lim
          then do
            writeIORef len n'
            return b
          else go n'
  r <- parse (go =<< readIORef len)
  n <- readIORef len
  return $ if n <= lim then Right r else Left n

parserChunks :: AP.Parser a -> ChunkParser (AP.Result a)
parserChunks parser next = run (AP.parse parser) where
  run p = do
    b <- next
    let r = p b
    if BS.null b
      then return r
      else run $ AP.feed r


mapBackEnd :: (a -> b) -> BackEnd a -> BackEnd b
mapBackEnd f back param info next = f <$> back param info next

nullBackEnd :: BackEnd Word64
nullBackEnd _ _ = nullChunks

limitBackEnd :: Word64 -> BackEnd a -> BackEnd (Either Word64 a)
limitBackEnd lim back param info = limitChunks lim $ back param info

parserBackEnd :: AP.Parser a -> BackEnd (AP.Result a)
parserBackEnd parser _ _ = parserChunks parser


parseRequestChunks :: (MonadIO m, RequestM c m) => ChunkParser a -> m a
parseRequestChunks p = liftIO . p =<< peeks requestBody

limitRequestChunks :: (MonadIO m, RequestM c m) => Word64 -> ChunkParser a -> m (Either Word64 a)
limitRequestChunks lim p = do
  rq <- peek
  case requestBodyLength rq of
    KnownLength l | l > lim -> return $ Left l
    _ -> liftIO $ limitChunks lim p $ requestBody rq

data Content
  = ContentForm 
    { contentFormParams :: [Param]
    , contentFormFiles :: [File Word64]
    }
  | ContentJSON JSON.Value
  | ContentTooLarge Word64
  | ContentUnknown

maxTextSize :: Word64
maxTextSize = 1024*1024

parseFormContent :: (MonadIO m, RequestM c m) => RequestBodyType -> m Content
parseFormContent t = either ContentTooLarge (uncurry ContentForm)
  <$> limitRequestChunks maxTextSize (sinkRequestBody nullBackEnd t)

parseJSONContent :: (MonadIO m, RequestM c m) => m Content
parseJSONContent = either ContentTooLarge (maybe ContentUnknown ContentJSON . AP.maybeResult)
  <$> limitRequestChunks maxTextSize (parserChunks JSON.json)

parseRequestContent :: (MonadIO m, RequestM c m) => m Content
parseRequestContent = do
  ct <- getRequestHeader "content-type"
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
