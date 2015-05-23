module Databrary.Store
  ( RawFilePath
  , rawFilePath
  , unRawFilePath
  , makeRelative
  , fileInfo
  , removeFile
  , sameFile
  , hashFile
  ) where

import Control.Arrow ((&&&))
import Control.Exception (handleJust)
import Control.Monad (guard, liftM2)
import Crypto.Hash (HashAlgorithm, hashInit, hashUpdate, hashFinalize, Digest)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import qualified System.FilePath as FP
import System.IO (withBinaryFile, IOMode(ReadMode))
import System.Posix.FilePath (RawFilePath)
import System.Posix.Files.ByteString (getFileStatus, isRegularFile, fileSize, modificationTimeHiRes, deviceID, fileID, removeLink)
import System.Posix.Types (FileOffset)
import System.IO.Error (isDoesNotExistError)
import System.IO.Unsafe (unsafeDupablePerformIO)

import Databrary.Ops
import Databrary.Model.Time

rawFilePath :: FilePath -> RawFilePath
rawFilePath s = unsafeDupablePerformIO $ do
  enc <- getFileSystemEncoding
  GHC.withCStringLen enc s BS.packCStringLen

unRawFilePath :: RawFilePath -> FilePath
unRawFilePath b = unsafeDupablePerformIO $ do
  enc <- getFileSystemEncoding
  BS.useAsCStringLen b $ GHC.peekCStringLen enc

makeRelative :: RawFilePath -> RawFilePath -> RawFilePath
makeRelative a b = rawFilePath $ FP.makeRelative (unRawFilePath a) (unRawFilePath b)

catchDoesNotExist :: IO a -> IO (Maybe a)
catchDoesNotExist f = handleJust (guard . isDoesNotExistError) (\_ -> return Nothing) $ Just <$> f

fileInfo :: RawFilePath -> IO (Maybe (FileOffset, Timestamp))
fileInfo f =
  (=<<) (liftM2 (?>) isRegularFile $ fileSize &&& posixSecondsToUTCTime . modificationTimeHiRes)
  <$> catchDoesNotExist (getFileStatus f)

removeFile :: RawFilePath -> IO Bool
removeFile f = isJust <$> catchDoesNotExist (removeLink f)

sameFile :: RawFilePath -> RawFilePath -> IO Bool
sameFile f1 f2 = do
  s1 <- getFileStatus f1
  s2 <- getFileStatus f2
  if deviceID s1 == deviceID s2 && fileID s1 == fileID s2
    then return True
    else withBinaryFile (unRawFilePath f1) ReadMode $ \h1 -> withBinaryFile (unRawFilePath f2) ReadMode $ cmp h1 where
  cmp h1 h2 = do
    b1 <- BS.hGet h1 defaultChunkSize
    b2 <- BS.hGet h2 defaultChunkSize
    if b1 == b2
      then if BS.null b1 then return True else cmp h1 h2
      else return False

hashFile :: HashAlgorithm a => RawFilePath -> IO (Digest a)
hashFile f = withBinaryFile (unRawFilePath f) ReadMode $ run hashInit where
  run s h = do
    b <- BS.hGetSome h defaultChunkSize
    if BS.null b
      then return $! hashFinalize s
      else (run $! hashUpdate s b) h

