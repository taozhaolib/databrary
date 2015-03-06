{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Upload
  ( uploadStart
  , uploadChunk
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import Control.Monad ((<=<), when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS.Unsafe
import Data.Int (Int64)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (castPtr)
import Network.HTTP.Types (noContent204, badRequest400)
import qualified Network.Wai as Wai
import System.IO (SeekMode(AbsoluteSeek))
import System.Posix.Files.ByteString (setFdSize)
import System.Posix.IO.ByteString (openFd, OpenMode(WriteOnly), defaultFileFlags, exclusive, closeFd, fdWriteBuf, fdSeek)
import System.Posix.Types (FileOffset, COff(..), ByteCount)

import Control.Has (view, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Format
import Databrary.Model.Token
import Databrary.Store.Upload
import Databrary.Web.Form.Deform
import Databrary.Action
import Databrary.Controller.Form
import Databrary.Controller.Volume

fileSizeForm :: (Functor m, Monad m) => DeformT m Int64
fileSizeForm = deformCheck "Invalid file size." (0 <) =<< deform

uploadStart :: Id Volume -> AppRAction
uploadStart vi = action POST (JSON, vi, "upload" :: T.Text) $ do
  withVolume PermissionEDIT vi $ \vol -> do
    (filename, size) <- runForm Nothing $ (,)
      <$> ("filename" .:> (deformCheck "File format not supported." (isJust . getFormatByFilename) =<< deform))
      <*> ("size" .:> fileSizeForm)
    tok <- createUpload vol filename size
    file <- peeks $ uploadFile tok
    liftIO $ bracket
      (openFd file WriteOnly (Just 0600) defaultFileFlags{ exclusive = True })
      closeFd
      (`setFdSize` COff size)
    okResponse [] $ unId (view tok :: Id Token)

chunkForm :: DeformT (ReaderT AuthRequest IO) (Upload, FileOffset, ByteCount)
chunkForm = do
  up <- "flowIdentifier" .:> (lift . (maybeAction <=< lookupUpload) =<< deform)
  "flowFilename" .:> (deformGuard "Filename mismatch." . (uploadFilename up ==) =<< deform)
  "flowTotalSize" .:> (deformGuard "File size mismatch." . (uploadSize up ==) =<< fileSizeForm)
  c <- "flowChunkSize" .:> (deformCheck "Chunk size too small." (1024 <=) =<< deform)
  i <- "flowChunkNumber" .:> (deformCheck "Chunk number out of range." (\i -> 0 <= i && c * i < uploadSize up) . pred =<< deform)
  let o = c * i
  l <- "flowCurrentChunkSize" .:> (deformCheck "Current chunk size out of range." (\l -> c <= l && o + l <= uploadSize up) =<< deform)
  return (up, COff o, fromIntegral l)

uploadChunk :: AppRAction
uploadChunk = action POST (JSON, "upload" :: T.Text) $ withAuth $ do
  (up, off, len) <- runForm Nothing chunkForm
  file <- peeks $ uploadFile up
  let badLength = result =<< returnResponse badRequest400 [] ("Incorrect content length." :: JSON.Value)
  bl <- peeks Wai.requestBodyLength
  case bl of
    Wai.KnownLength l | CSize l /= len -> badLength
    _ -> return ()
  -- TODO: clear on error (maybe wait for calloc)
  rb <- peeks Wai.requestBody
  n <- liftIO $ bracket
    (openFd file WriteOnly Nothing defaultFileFlags)
    closeFd
    (\fd -> do
      let block n = do
            b <- rb
            if BS.null b
              then return n
              else do
                let c = fromIntegral $ BS.length b
                    n' = n + c
                if n' > len
                  then return n'
                  else do
                    r <- BS.Unsafe.unsafeUseAsCString b $ \p ->
                      fdWriteBuf fd (castPtr p) c

                    block n'
      fdSeek fd AbsoluteSeek off
      block 0)
  when (n /= len) badLength
  emptyResponse noContent204 []
