{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}
module Databrary.Media.AV
  ( AVError(..)
  , AV
  , initAV
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCAString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.StablePtr
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafeDupablePerformIO)

#include <libavformat/avformat.h>

foreign import ccall unsafe "libavutil/error.h av_strerror"
  avStrerror :: CInt -> CString -> CSize -> IO CInt

avShowError :: CInt -> String
avShowError e = unsafeDupablePerformIO $
  allocaBytes len $ \p -> do
    avStrerror e p (fromIntegral len)
    peekCAString p
  where len = 256

data AVError = AVError
  { avErrorCode :: CInt
  } deriving (Typeable)

instance Exception AVError

instance Show AVError where
  showsPrec p (AVError e) = showParen (p > 10) $
    showString "AVError " . showString (avShowError e)

throwAVErrorIfNegative :: IO CInt -> IO CInt
throwAVErrorIfNegative f = do
  r <- f
  when (r < 0) $ throwIO $ AVError r
  return r

type AVLockOp = #type enum AVLockOp
type AVLockmgr a = Ptr (Ptr a) -> AVLockOp -> IO CInt

foreign import ccall safe "libavcodec/avcodec.h av_lockmgr_register"
  avLockmgrRegister :: FunPtr (AVLockmgr a) -> IO CInt

foreign import ccall "wrapper"
  mkAVLockmgr :: AVLockmgr a -> IO (FunPtr (AVLockmgr a))

foreign import ccall safe "libavformat/avformat.h av_register_all"
  avRegisterAll :: IO ()

avLockmgr :: AVLockmgr ()
avLockmgr p o
  | o == #{const AV_LOCK_CREATE} = (<$) 0 $
    poke p . castStablePtrToPtr =<< newStablePtr =<< newMVar ()
  | o == #{const AV_LOCK_OBTAIN} = (<$) 0 $
    takeMVar =<< deRefStablePtr =<< s
  | o == #{const AV_LOCK_RELEASE} = (<$) 0 $
    (`putMVar` ()) =<< deRefStablePtr =<< s
  | o == #{const AV_LOCK_DESTROY} = (<$) 0 $
    freeStablePtr =<< s
  | otherwise = return (-1)
  where
  s = castPtrToStablePtr <$> peek p
  s :: IO (StablePtr (MVar ()))

data AV = AV 

initAV :: IO AV
initAV = do
  avRegisterAll
  mgr <- mkAVLockmgr avLockmgr
  throwAVErrorIfNegative $ avLockmgrRegister mgr
  -- leak mgr
  return AV
