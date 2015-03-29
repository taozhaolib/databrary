module Databrary.Model.Transcode.Types
  ( Transcode(..)
  ) where

import qualified Data.ByteString as BS
import Data.Int (Int32)
import qualified Data.Text as T

import Databrary.Model.Segment
import Databrary.Model.Asset.Types
import Databrary.Model.Party.Types

data Transcode = Transcode
  { transcodeAsset :: Asset
  , transcodeOwner :: Party
  , transcodeOrig :: Asset
  , transcodeSegment :: Segment
  , transcodeOptions :: [BS.ByteString]
  , transcodeProcess :: Maybe Int32
  , transcodeLog :: Maybe T.Text
  }

