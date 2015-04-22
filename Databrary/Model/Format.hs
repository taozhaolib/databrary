{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}
module Databrary.Model.Format
  ( module Databrary.Model.Format.Types
  , unknownFormat
  , allFormats
  , getFormat
  , getFormat'
  , getFormatByFilename
  , dropFormatExtension
  , videoFormat
  , imageFormat
  , formatIsImage
  , formatTranscodable
  , formatSample
  , formatJSON
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Char (toLower)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import System.Posix.FilePath (RawFilePath, splitExtension, takeExtension)

import Databrary.Ops
import qualified Databrary.JSON as JSON
import Databrary.Model.Id
import Databrary.Model.Format.Types
import Databrary.Model.Format.Boot

unknownFormat :: Format
unknownFormat = Format
  { formatId = error "unknownFormat"
  , formatMimeType = "application/octet-stream"
  , formatExtension = []
  , formatName = "Unknown"
  }

allFormats :: [Format]
allFormats = $(loadFormats)

formatsById :: IntMap.IntMap Format
formatsById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ formatId a, a)) allFormats

getFormat :: Id Format -> Maybe Format
getFormat (Id i) = IntMap.lookup (fromIntegral i) formatsById

getFormat' :: Id Format -> Format
getFormat' (Id i) = formatsById IntMap.! fromIntegral i

formatsByExtension :: Map.Map BS.ByteString Format
formatsByExtension = Map.fromList [ (e, a) | a <- allFormats, e <- formatExtension a ]

getFormatByExtension :: BS.ByteString -> Maybe Format
getFormatByExtension e = Map.lookup (BSC.map toLower e) formatsByExtension

getFormatByFilename :: RawFilePath -> Maybe Format
getFormatByFilename n = do
  ('.',e) <- BSC.uncons $ takeExtension n
  getFormatByExtension e

dropFormatExtension :: Format -> RawFilePath -> RawFilePath
dropFormatExtension fmt n
  | (f,e) <- splitExtension n
  , BSC.map toLower e `elem` formatExtension fmt = f
  | otherwise = n

videoFormat :: Format
videoFormat = getFormat' (Id (-800))

imageFormat :: Format
imageFormat = getFormat' (Id (-700))

formatIsVideo :: Format -> Bool
formatIsVideo Format{ formatMimeType = t } = "video/" `BS.isPrefixOf` t

formatIsImage :: Format -> Bool
formatIsImage Format{ formatMimeType = t } = "image/" `BS.isPrefixOf` t

formatTranscodable :: Format -> Maybe Format
formatTranscodable f
  | formatIsVideo f = Just videoFormat
  | otherwise = Nothing

formatSample :: Format -> Maybe Format
formatSample f
  | f == videoFormat = Just imageFormat
  | otherwise = Nothing

formatJSON :: Format -> JSON.Object
formatJSON Format{..} = JSON.record formatId $ catMaybes
  [ Just $ "mimetype" JSON..= formatMimeType
  , null formatExtension ?!> "extension" JSON..= formatExtension
  , Just $ "name" JSON..= formatName
  -- TODO: description, transcodable
  ]
