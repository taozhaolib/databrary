module Databrary.Model.Transcode.Types
  ( Transcode(..)
  , TranscodeOptions
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import qualified Data.Text as T

import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Party.Types

type TranscodeOptions = [BS.ByteString]

data Transcode = Transcode
  { transcodeAsset :: Asset
  , transcodeOwner :: Account
  , transcodeOrig :: Asset
  , transcodeSegment :: Segment
  , transcodeOptions :: TranscodeOptions
  , transcodeProcess :: Maybe Int32
  , transcodeLog :: Maybe T.Text
  }

