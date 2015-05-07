{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Upload
  ( uploadStart
  , uploadChunk
  , testChunk
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import Control.Monad ((<=<), when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Word (Word64)
import Network.HTTP.Types (ok200, noContent204, badRequest400)
import qualified Network.Wai as Wai
import System.IO (withFile, SeekMode(AbsoluteSeek), IOMode(WriteMode, ReadMode), hSeek)
import System.Posix.Files.ByteString (setFdSize)
import System.Posix.IO.ByteString (openFd, OpenMode(WriteOnly), defaultFileFlags, exclusive, closeFd)
import System.Posix.Types (COff(..))

import Databrary.Has (view, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.Model.Format
import Databrary.Model.Token
import Databrary.Store
import Databrary.Store.Upload
import Databrary.Store.Asset
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Form
import Databrary.Controller.Volume

fileSizeForm :: (Functor m, Monad m) => DeformT m Int64
fileSizeForm = deformCheck "Invalid file size." (0 <) =<< deform

uploadStart :: AppRoute (Id Volume)
uploadStart = action POST (pathJSON >/> pathId </< "upload") $ \vi -> withAuth $ do
  vol <- getVolume PermissionEDIT vi
  (filename, size) <- runForm Nothing $ (,)
    <$> ("filename" .:> (deformCheck "File format not supported." (isJust . getFormatByFilename) =<< deform))
    <*> ("size" .:> (deformCheck "File too large." ((maxAssetSize >=) . fromIntegral) =<< fileSizeForm))
  tok <- createUpload vol filename size
  file <- peeks $ uploadFile tok
  liftIO $ bracket
    (openFd file WriteOnly (Just 0600) defaultFileFlags{ exclusive = True })
    closeFd
    (`setFdSize` COff size)
  okResponse [] $ unId (view tok :: Id Token)

chunkForm :: DeformT (ReaderT AuthRequest IO) (Upload, Int64, Word64)
chunkForm = do
  up <- "flowIdentifier" .:> (lift . (maybeAction <=< lookupUpload) =<< deform)
  "flowFilename" .:> (deformGuard "Filename mismatch." . (uploadFilename up ==) =<< deform)
  "flowTotalSize" .:> (deformGuard "File size mismatch." . (uploadSize up ==) =<< fileSizeForm)
  c <- "flowChunkSize" .:> (deformCheck "Chunk size too small." (1024 <=) =<< deform)
  i <- "flowChunkNumber" .:> (deformCheck "Chunk number out of range." (\i -> 0 <= i && c * i < uploadSize up) . pred =<< deform)
  let o = c * i
  l <- "flowCurrentChunkSize" .:> (deformCheck "Current chunk size out of range." (\l -> c <= l && o + l <= uploadSize up) =<< deform)
  return (up, o, fromIntegral l)

uploadChunk :: AppRoute ()
uploadChunk = action POST (pathJSON </< "upload") $ \() -> withAuth $ do
  (up, off, len) <- runForm Nothing chunkForm
  file <- peeks $ uploadFile up
  let badLength = result =<< returnResponse badRequest400 [] ("Incorrect content length." :: JSON.Value)
  bl <- peeks Wai.requestBodyLength
  case bl of
    Wai.KnownLength l | l /= len -> badLength
    _ -> return ()
  rb <- peeks Wai.requestBody
  n <- liftIO $ withFile (unRawFilePath file) WriteMode $ \h -> do
    hSeek h AbsoluteSeek (toInteger off)
    let block n = do
          b <- rb
          if BS.null b
            then return n
            else do
              let n' = n + fromIntegral (BS.length b)
              if n' > len
                then return n'
                else do
                  BS.hPut h b
                  block n'
    block 0
  when (n /= len) $ do
    -- TODO: clear block (maybe wait for calloc)
    badLength
  emptyResponse noContent204 []

testChunk :: AppRoute ()
testChunk = action GET (pathJSON </< "upload") $ \() -> withAuth $ do
  (up, off, len) <- runForm Nothing chunkForm
  file <- peeks $ uploadFile up
  r <- liftIO $ withFile (unRawFilePath file) ReadMode $ \h -> do
    hSeek h AbsoluteSeek (toInteger off)
    let block 0 = return False
        block n = do
          b <- BS.hGetSome h $ fromIntegral $ n `min` fromIntegral defaultChunkSize
          if BS.any (0 /=) b
            then return True
            else block $ n - fromIntegral (BS.length b)
    block len
  emptyResponse (if r then ok200 else noContent204) []
