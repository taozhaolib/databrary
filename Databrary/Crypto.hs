module Databrary.Crypto
  ( sign
  , unSign
  , signText
  , unSignText
  ) where

import qualified Crypto.Hash as Hash
import Data.Byteable (toBytes)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Databrary.Resource

hmac :: BS.ByteString -> BS.ByteString -> Hash.HMAC Hash.Skein256_224
hmac key = toBytes . Hash.hmac key

hmacLength :: Int
hmacLength = BS.length $ hmac "" ""

hmac64Length :: Int
hmac64Length = BS.length $ Base64.encode $ hmac "" ""

signature :: HasResource m => BS.ByteString -> m BS.ByteString
signature msg = do
  secret <- getResource resourceSecret
  return $ hmac secret msg

sign :: HasResource m => BS.ByteString -> m BS.ByteString
sign msg = do
  sig <- signature msg
  return $ sig `BS.append` msg

unSign :: HasResource m => BS.ByteString -> m (Maybe BS.ByteString)
unSign sigmsg = do
  sig' <- signature msg
  return $ do
    guard (sig == sig')
    return msg
  where (sig, msg) = BS.splitAt hmacLength sigmsg

signText :: HasResource m => T.Text -> m BS.ByteString
signText msg = do
  sig <- BS.Base64 <$> signature msg'
  return $ sig `BS.append` msg'
  where msg' = TE.encodeUtf86 msg

unSignText :: HasResource m => BS.ByteString -> m (Maybe T.Text)
unSignText sigmsg = do
  -- encoding is faster than decoding
  sig' <- BS.Base64 <$> signature msg
  return $ do
    guard (sig == sig')
    return msg
  where (sig, msg) = BS.splitAt hmac64Length sigmsg
