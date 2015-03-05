{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Upload
  ( uploadStart
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (isJust)
import qualified Data.Text as T
import System.Posix.Files.ByteString (setFdSize)
import System.Posix.IO.ByteString (openFd, OpenMode(WriteOnly), defaultFileFlags, exclusive, closeFd)
import System.Posix.Types (COff(..))

import Control.Has (view, peeks)
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

uploadStart :: Id Volume -> AppRAction
uploadStart vi = action POST (JSON, vi, "asset" :: T.Text) $ do
  withVolume PermissionEDIT vi $ \vol -> do
    (filename, size) <- runForm Nothing $ (,)
      <$> ("filename" .:> (deformCheck "File format not supported." (isJust . getFormatByFilename) =<< deform))
      <*> ("size" .:> (deformCheck "Invalid file size." (0 <) =<< deform))
    tok <- createUpload vol filename
    file <- peeks $ uploadFile tok
    liftIO $ bracket
      (openFd file WriteOnly (Just 0600) defaultFileFlags{ exclusive = True })
      closeFd
      (`setFdSize` COff size)
    okResponse [] $ unId (view tok :: Id Token)
