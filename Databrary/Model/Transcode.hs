{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell, QuasiQuotes #-}
module Databrary.Model.Transcode
  ( module Databrary.Model.Transcode.Types
  , transcodeAuth
  , lookupTranscode
  , addTranscode
  , updateTranscode
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Database.PostgreSQL.Typed.Query (pgSQL)

import Databrary.DB
import Databrary.Has (peek)
import Databrary.Resource
import Databrary.Crypto
import Databrary.Store.Types
import Databrary.Model.SQL
import Databrary.Model.Audit
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Offset
import Databrary.Model.Segment
import Databrary.Model.Format
import Databrary.Model.Asset
import Databrary.Model.Transcode.Types
import Databrary.Model.Transcode.SQL

defaultTranscodeOptions :: TranscodeArgs
defaultTranscodeOptions = ["-vf", "pad=iw+mod(iw\\,2):ih+mod(ih\\,2)"]

transcodeAuth :: MonadHasResource c m => Transcode -> m BS.ByteString
transcodeAuth t = signature $ BSL.toStrict $ BSB.toLazyByteString
  $ maybe id ((<>) . BSB.byteString) (assetSHA1 $ transcodeOrig t)
  $ BSB.int32LE (unId $ transcodeId t)

lookupTranscode :: DBM m => Id Transcode -> m (Maybe Transcode)
lookupTranscode a =
  dbQuery1 $(selectQuery selectTranscode "WHERE transcode.asset = ${a}")

addTranscode :: (MonadHasSiteAuth c m, MonadAudit c m, MonadStorage c m) => Asset -> Segment -> TranscodeArgs -> Maybe Offset -> m Transcode
addTranscode orig seg@(Segment rng) opts dur = do
  own <- peek
  let Just fmt = formatTranscodable (assetFormat orig)
  a <- addAsset orig
    { assetFormat = fmt
    , assetDuration = dur
    , assetSHA1 = Nothing
    , assetSize = Nothing
    } Nothing
  dbExecute1' [pgSQL|INSERT INTO transcode (asset, owner, orig, segment, options) VALUES (${assetId a}, ${partyId $ accountParty $ siteAccount own}, ${assetId orig}, ${seg}, ${map Just opts})|]
  dbExecute1 [pgSQL|UPDATE slot_asset SET asset = ${assetId a}, segment = segment(lower(segment) + ${fromMaybe 0 $ lowerBound rng}, COALESCE(lower(segment) + ${upperBound rng}, upper(segment))) WHERE asset = ${assetId orig}|]
  return Transcode
    { transcodeAsset = a
    , transcodeOwner = own
    , transcodeOrig = orig
    , transcodeSegment = seg
    , transcodeOptions = opts
    , transcodeProcess = Nothing
    , transcodeLog = Nothing
    }

updateTranscode :: DBM m => Transcode -> Maybe TranscodePID -> Maybe String -> m Transcode
updateTranscode tc pid logs = do
  r <- dbQuery1 [pgSQL|UPDATE transcode SET process = ${pid}, log = COALESCE(COALESCE(log || E'\\n', '') || ${logs}, log) WHERE asset = ${assetId $ transcodeAsset tc} AND COALESCE(process, 0) = ${fromMaybe 0 $ transcodeProcess tc} RETURNING log|]
  return $ maybe tc (\l -> tc
    { transcodeProcess = pid
    , transcodeLog = l
    }) r
