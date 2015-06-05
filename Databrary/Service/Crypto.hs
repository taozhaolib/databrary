{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Crypto
  ( signature
  , sign
  , unSign
  , constEqBytes
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Crypto.Hash as Hash
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as Base64
import Data.Byteable (toBytes, constEqBytes)
import Data.Monoid ((<>))

import Databrary.Ops
import Databrary.Has (peeks, focusIO)
import Databrary.Service.Types
import Databrary.Service.Entropy

hmac :: BS.ByteString -> BS.ByteString -> BS.ByteString
hmac key = Base64.encode . (toBytes :: Hash.HMAC Hash.Skein256_224 -> BS.ByteString) . Hash.hmac key

hmacLength :: Int
hmacLength = BS.length $ hmac "" ""

signature :: BS.ByteString -> Secret -> BS.ByteString
signature msg (Secret secret) = hmac secret msg

nonceBytes, nonceLength :: Int
nonceBytes = 6
nonceLength = BS.length $ Base64.encode $ BS.replicate nonceBytes 0 -- 8

sign :: (MonadHasService c m, MonadIO m) => BS.ByteString -> m BS.ByteString
sign msg = do
  nonce <- focusIO $ entropyBytes nonceBytes
  sig <- peeks $ signature (msg <> nonce)
  return $ sig <> Base64.encode nonce <> msg

unSign :: MonadHasService c m => BS.ByteString -> m (Maybe BS.ByteString)
unSign sigmsg = do
  sig' <- peeks $ signature (msg <> nonce)
  return $ constEqBytes sig sig' ?> msg
  where
  (sig, noncemsg) = BS.splitAt hmacLength sigmsg
  (nonce64, msg) = BS.splitAt nonceLength noncemsg
  nonce = Base64.decodeLenient nonce64
