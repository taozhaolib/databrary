{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Databrary.Passwd
  ( passwordPolicy
  , passwdCheck
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent.QSem
import Control.Exception (bracket_)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString as BS
import Foreign.C.String (CString)
import Foreign.Ptr (nullPtr)
import System.IO.Unsafe (unsafePerformIO)

passwordPolicy :: BCrypt.HashingPolicy
passwordPolicy = BCrypt.HashingPolicy
  { BCrypt.preferredHashAlgorithm = "$2b$"
  , BCrypt.preferredHashCost = 12
  }

foreign import ccall unsafe "crack.h FascistCheckUser"
  fascistCheckUser :: CString -> CString -> CString -> CString -> IO CString

{-# NOINLINE crackSem #-}
crackSem :: QSem
crackSem = unsafePerformIO $ newQSem 1

passwdCheck :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Maybe BS.ByteString)
passwdCheck passwd user name =
  bracket_ (waitQSem crackSem) (signalQSem crackSem) $
    BS.useAsCString passwd $ \p ->
      BS.useAsCString user $ \u ->
        BS.useAsCString name $ \n -> do
          r <- fascistCheckUser p nullPtr u n
          if r == nullPtr
            then return Nothing
            else Just <$> BS.packCString r
