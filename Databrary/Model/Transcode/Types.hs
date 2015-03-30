module Databrary.Model.Transcode.Types
  ( Transcode(..)
  , TranscodePID
  , TranscodeArgs
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int32)

import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Party.Types

type TranscodePID = Int32
type TranscodeArgs = [String]

data Transcode = Transcode
  { transcodeAsset :: Asset
  , transcodeOwner :: Account
  , transcodeOrig :: Asset
  , transcodeSegment :: Segment
  , transcodeOptions :: TranscodeArgs
  , transcodeProcess :: Maybe TranscodePID
  , transcodeLog :: Maybe BS.ByteString
  }

