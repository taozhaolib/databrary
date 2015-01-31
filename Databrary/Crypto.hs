{-# LANGUAGE OverloadedStrings #-}
module Databrary.Crypto
  ( sign
  , unSign
  ) where

import Control.Monad (guard)
import qualified Crypto.Hash as Hash
import Data.Byteable (toBytes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64

import Databrary.Resource

hmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac key = Base64.encode . (toBytes :: Hash.HMAC Hash.Skein256_224 -> BS.ByteString) . Hash.hmac key

hmacLength :: Int
hmacLength = BS.length $ hmac "" ""

signature :: ResourceM c m => BS.ByteString -> m BS.ByteString
signature msg = do
  secret <- getResource resourceSecret
  return $ hmac secret msg

sign :: ResourceM c m => BS.ByteString -> m BS.ByteString
sign msg = do
  sig <- signature msg
  return $ sig `BS.append` msg

unSign :: ResourceM c m => BS.ByteString -> m (Maybe BS.ByteString)
unSign sigmsg = do
  sig' <- signature msg
  return $ do
    guard (sig == sig')
    return msg
  where (sig, msg) = BS.splitAt hmacLength sigmsg
