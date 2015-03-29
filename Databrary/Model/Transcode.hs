{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Databrary.Model.Transcode
  ( module Databrary.Model.Transcode.Types
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)

import Databrary.Store.Storage
import Databrary.Store.Asset
import Databrary.Model.Segment
import Databrary.Model.Transcode.Types

transcodeArgs :: MonadStorage c m => Transcode -> m [BS.ByteString]
transcodeArgs Transcode{..} = do
  Just f <- getAssetFile transcodeOrig
  return $
    [ "-f", f
    , "-r" -- , actionURL ...
    , "--" ]
    ++ maybe [] (\l -> ["-ss", BSC.pack $ show l]) lb
    ++ maybe [] (\u -> ["-t", BSC.pack $ show $ u - fromMaybe 0 lb]) (upperBound rng)
    ++ transcodeOptions
  where
  rng = segmentRange transcodeSegment
  lb = lowerBound rng
