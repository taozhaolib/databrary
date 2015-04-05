{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, EmptyDataDecls, OverloadedStrings #-}
module Databrary.Media.AV
  ( AVError(..)
  , avErrorString
  , AV
  , initAV
  , AVProbe(..)
  , avProbe
  , avProbeLength
  , avProbeIsVideo
  ) where

import Control.Applicative ((<*>))
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (Exception, throwIO, bracket_)
import Control.Monad (void, when, forM)
import qualified Data.ByteString as BS
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Data.Ratio ((%))
import Data.Time.Clock (DiffTime)
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.C.String (CString, peekCAString)
import Foreign.C.Types (CInt(..), CUInt(..), CSize(..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.StablePtr
import Foreign.Storable (peek, poke, peekByteOff, peekElemOff)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Databrary.Ops
import Databrary.Store
import Databrary.Model.Offset

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
  , avErrorFunction :: String
  , avErrorFile :: Maybe RawFilePath
  } deriving (Typeable)

instance Exception AVError

avErrorString :: AVError -> String
avErrorString = avShowError . avErrorCode

instance Show AVError where
  showsPrec p (AVError e c f) = showParen (p > 10) $
    showString "AVError "
    . showString c
    . maybe id (((' ' :) .) . shows) f
    . showString ": "
    . showString (avShowError e)

data ErrorFile
  = NoFile
  | FileName RawFilePath
  | FileContext (Ptr AVFormatContext)

errorFile :: ErrorFile -> IO (Maybe RawFilePath)
errorFile NoFile = return Nothing
errorFile (FileName f) = return $ Just f
errorFile (FileContext a) = Just <$> (BS.packCString =<< #{peek AVFormatContext, filename} a)

throwAVErrorIf :: String -> ErrorFile -> IO CInt -> IO CInt
throwAVErrorIf c f g = do
  r <- g
  when (r < 0) $
    throwIO . AVError r c =<< errorFile f
  return r

throwAVErrorIf_ :: String -> ErrorFile -> IO CInt -> IO ()
throwAVErrorIf_ c f = void . throwAVErrorIf c f

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
  throwAVErrorIf "av_lockmgr_register" NoFile $ avLockmgrRegister mgr
  -- leak mgr
  return AV

data AVFormatContext
data AVInputFormat
data AVDictionary

foreign import ccall safe "libavformat/avformat.h avformat_open_input"
  avformatOpenInput :: Ptr (Ptr AVFormatContext) -> CString -> Ptr AVInputFormat -> Ptr (Ptr AVDictionary) -> IO CInt

foreign import ccall safe "libavformat/avformat.h avformat_close_input"
  avformatCloseInput :: Ptr (Ptr AVFormatContext) -> IO ()

withAVInput :: AV -> RawFilePath -> (Ptr AVFormatContext -> IO a) -> IO a
withAVInput AV f g =
  BS.useAsCString f $ \cf ->
    with nullPtr $ \a ->
      bracket_
        (throwAVErrorIf "avformat_open_input" (FileName f) $ avformatOpenInput a cf nullPtr nullPtr)
        (avformatCloseInput a)
        (g =<< peek a)

foreign import ccall safe "libavformat/avformat.h avformat_find_stream_info"
  avformatFindStreamInfo :: Ptr AVFormatContext -> Ptr (Ptr AVDictionary) -> IO CInt

findAVStreamInfo :: Ptr AVFormatContext -> IO ()
findAVStreamInfo a = throwAVErrorIf_ "avformat_find_stream_info" (FileContext a) $
  avformatFindStreamInfo a nullPtr

foreign import ccall unsafe "libavcodec/avcodec.h avcodec_get_name"
  avcodecGetName :: #{type enum AVCodecID} -> IO CString

data AVProbe = AVProbe
  { avProbeFormat :: BS.ByteString
  , avProbeDuration :: DiffTime
  , avProbeStreams :: [BS.ByteString]
  }

-- |Test if this represents a video in standard format.
avProbeIsVideo :: AVProbe -> Bool
avProbeIsVideo AVProbe{ avProbeFormat = "mov,mp4,m4a,3gp,3g2,mj2", avProbeStreams = ("h264":s) } =
  s `isPrefixOf` ["aac"]
avProbeIsVideo _ = False

avProbeLength :: AVProbe -> Maybe Offset
avProbeLength AVProbe{ avProbeDuration = o } = o > 0 ?> Offset o

avTime :: Int64 -> DiffTime
avTime t = realToFrac $ t % #{const AV_TIME_BASE}

avProbe :: AV -> RawFilePath -> IO AVProbe
avProbe av f =
  withAVInput av f $ \ic -> do
    findAVStreamInfo ic
    AVProbe
      <$> (BS.packCString =<< #{peek AVInputFormat, name} =<< #{peek AVFormatContext, iformat} ic)
      <*> (avTime <$> #{peek AVFormatContext, duration} ic)
      <*> do
        nb :: CUInt <- #{peek AVFormatContext, nb_streams} ic
        ss <- #{peek AVFormatContext, streams} ic
        forM [0..pred (fromIntegral nb)] $ \i ->
          BS.packCString =<< avcodecGetName =<< #{peek AVCodecContext, codec_id} =<< #{peek AVStream, codec} =<< peekElemOff ss i
