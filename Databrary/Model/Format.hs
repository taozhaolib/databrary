{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Databrary.Model.Format
  ( module Databrary.Model.Format.Types
  , unknownFormat
  , getFormat
  , getFormat'
  , getFormatByFilename
  ) where

import Data.Char (toLower)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import System.FilePath (takeExtension)

import Databrary.Model.Id
import Databrary.Model.Format.Types
import Databrary.Model.Format.Boot

unknownFormat :: Format
unknownFormat = Format
  { formatId = error "unknown format"
  , formatMimeType = "application/octet-stream"
  , formatExtension = []
  , formatName = "Unknown"
  }

formats :: [Format]
formats = $(loadFormats)

formatsById :: IntMap.IntMap Format
formatsById = IntMap.fromAscList $ map (\a -> (fromIntegral $ unId $ formatId a, a)) formats

getFormat :: Id Format -> Maybe Format
getFormat (Id i) = IntMap.lookup (fromIntegral i) formatsById

getFormat' :: Id Format -> Format
getFormat' (Id i) = formatsById IntMap.! fromIntegral i

formatsByExtension :: Map.Map String Format
formatsByExtension = Map.fromList [ (e, a) | a <- formats, e <- formatExtension a ]

getFormatByExtension :: String -> Maybe Format
getFormatByExtension e = Map.lookup (map toLower e) formatsByExtension

getFormatByFilename :: FilePath -> Maybe Format
getFormatByFilename = me . takeExtension where
  me ('.':e) = getFormatByExtension e
  me _ = Nothing
