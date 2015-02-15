module Databrary.Store.Asset
  ( getAssetFile
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

assetFile :: Asset -> Maybe FilePath
assetFile = fmap sf . BS.uncons <=< assetSHA1 where
  sf (h,t) = bs (BSB.word8HexFixed h) </> bs (BSB.byteStringHex t)
  bs = BSLC.unpack . BSB.toLazyByteString

getAssetFile :: StorageM c m => Asset -> m (Maybe FilePath)
getAssetFile a = do
  s <- peek
  let 
    mf Nothing p = return $ storageMaster s </> p
    mf (Just sf) p = do
      me <- fileExist m
      if me
        then return m
        else do
          fe <- fileExist f
          return $ if fe then f else m
      where
      m = storageMaster s </> p
      f = sf </> p
  Trav.mapM (liftIO . mf (storageFallback s)) $ assetFile a
