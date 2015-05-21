{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings, TemplateHaskell #-}
module Databrary.Service.Passwd
  ( passwordPolicy
  , passwdCheck
  , Passwd
  , MonadHasPasswd
  , initPasswd
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Crypto.BCrypt as BCrypt
import qualified Data.ByteString as BS
import Foreign.C.String (CString)
import Foreign.Ptr (nullPtr)

import Databrary.Ops
import Databrary.Has (makeHasRec, peeks)

passwordPolicy :: BCrypt.HashingPolicy
passwordPolicy = BCrypt.HashingPolicy
  { BCrypt.preferredHashAlgorithm = "$2b$"
  , BCrypt.preferredHashCost = 12
  }

foreign import ccall unsafe "crack.h FascistCheckUser"
  fascistCheckUser :: CString -> CString -> CString -> CString -> IO CString

data Passwd = Passwd
  { passwdLock :: MVar ()
  }

makeHasRec ''Passwd []

initPasswd :: IO Passwd
initPasswd = Passwd <$> newMVar ()

passwdCheck :: (MonadHasPasswd c m, MonadIO m) => BS.ByteString -> BS.ByteString -> BS.ByteString -> m (Maybe BS.ByteString)
passwdCheck passwd user name = do
  lock <- peeks passwdLock
  liftIO $ withMVar lock $ \() ->
    BS.useAsCString passwd $ \p ->
      BS.useAsCString user $ \u ->
        BS.useAsCString name $ \n -> do
          r <- fascistCheckUser p nullPtr u n
          r /= nullPtr ?$> BS.packCString r
