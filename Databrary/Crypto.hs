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
import Data.Monoid ((<>))

import Control.Has (peeks)
import Databrary.Resource
import Databrary.Entropy

hmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac key = Base64.encode . (toBytes :: Hash.HMAC Hash.Skein256_224 -> BS.ByteString) . Hash.hmac key

hmacLength :: Int
hmacLength = BS.length $ hmac "" ""

signature :: MonadHasResource c m => BS.ByteString -> m BS.ByteString
signature msg = do
  secret <- peeks resourceSecret
  return $ hmac secret msg

nonceBytes, nonceLength :: Int
nonceBytes = 6
nonceLength = BS.length $ Base64.encode $ BS.replicate nonceBytes 0 -- 8

sign :: (MonadHasResource c m, EntropyM c m) => BS.ByteString -> m BS.ByteString
sign msg = do
  nonce <- entropyBytes nonceBytes
  sig <- signature (msg <> nonce)
  return $ sig <> Base64.encode nonce <> msg

unSign :: MonadHasResource c m => BS.ByteString -> m (Maybe BS.ByteString)
unSign sigmsg = do
  sig' <- signature (msg <> nonce)
  return $ do
    guard (sig == sig')
    return msg
  where
  (sig, noncemsg) = BS.splitAt hmacLength sigmsg
  (nonce64, msg) = BS.splitAt nonceLength noncemsg
  nonce = Base64.decodeLenient nonce64
