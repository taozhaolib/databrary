{-# LANGUAGE OverloadedStrings #-}
module Databrary.Store.AssetSegment
  ( assetSegmentTag
  , getAssetSegmentStore
  ) where

import Control.Monad (unless, liftM2)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import Data.Maybe (isJust, fromJust)
import Data.Monoid ((<>))
import Data.Word (Word16)
import qualified Database.PostgreSQL.Typed.Range as Range
import System.Posix.FilePath (takeDirectory)

import Databrary.Ops
import Databrary.Has (MonadHas, peek)
import Databrary.Files
import Databrary.Service.ResourceT
import Databrary.Model.Offset
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.AssetSlot
import Databrary.Model.AssetSegment
import Databrary.Store.Types
import Databrary.Store.Asset
import Databrary.Store.Temp
import Databrary.Store.AV

assetSegmentTag :: AssetSegment -> Maybe Word16 -> String
assetSegmentTag as sz = m ':' (assetSegmentFull as ?!> s) ++ m '@' (show <$> sz) where
  m = maybe "" . (:)
  c = assetSegmentRange as
  s = maybe (b (Range.lowerBound c) ++ '-' : b (Range.upperBound c)) (show . offsetMillis) (Range.getPoint c)
  b = maybe "" (show . offsetMillis) . Range.bound

assetSegmentFile :: AssetSegment -> Maybe Word16 -> Maybe RawFilePath
assetSegmentFile as sz = (<> BSC.pack (assetSegmentTag as sz)) <$> assetFile (slotAsset $ segmentAsset as)

type Stream = BS.ByteString -> IO ()

genVideoClip :: AV -> RawFilePath -> Maybe (Range.Range Offset) -> Maybe Word16 -> Either Stream RawFilePath -> IO ()
genVideoClip _ src (Just clip) _ dst | Nothing <- Range.getPoint clip = fail "not implemented"
genVideoClip av src frame sz dst =
  avFrame src (offsetTime <$> (Range.getPoint =<< frame)) sz Nothing (either (const Nothing) Just dst) av
    >>= Fold.mapM_ (\b -> send b >> send BS.empty) 
  where send = either id (const $ const $ return ()) dst

getAssetSegmentStore :: (MonadStorage c m, MonadHas AV c m, MonadResourceT c m) => AssetSegment -> Maybe Word16 -> m (Either (Stream -> IO ()) RawFilePath)
getAssetSegmentStore as sz 
  | aimg && isJust sz || not (assetSegmentFull as) && isJust (assetDuration a) && isJust (formatSample afmt) = do
  Just af <- getAssetFile a
  av <- peek
  store <- peek
  rs <- peek
  let cache = storageCache store
      cf = liftM2 (</>) cache $ assetSegmentFile as sz
      gen = genVideoClip av af (afmt == imageFormat ?!> clip) sz
  liftIO $ maybe
    (return $ Left $ gen . Left)
    (\f -> do
      fe <- fileExist f
      unless fe $ do
        tf <- makeTempFileAs (maybe (storageTemp store) (</> "tmp/") cache) (const $ return ()) rs
        gen (Right (tempFilePath tf))
        _ <- createDir (takeDirectory f) 0o770
        renameTempFile tf f rs
      return $ Right f)
    cf
  | otherwise = Right . fromJust <$> getAssetFile a
  where
  a = slotAsset $ segmentAsset as
  afmt = assetFormat a
  aimg = afmt == imageFormat
  clip = assetSegmentRange as
