module Databrary.Store.Upload
  ( uploadFile
  ) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Traversable as Trav
import System.FilePath ((</>))
import System.Posix.Files (fileExist)

import Control.Has (peek)
import Databrary.Store.Storage
import Databrary.Model.Asset

uploadFile :: UploadToken -> Storage -> FilePath
uploadFile t s = storageUpload s </> BSC.unpack (unId (view t :: Id Token))
