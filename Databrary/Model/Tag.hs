module Databrary.Model.Tag
  ( module Databrary.Model.Tag.Types
  , validateTag
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Text.Regex.Posix as Regex

import Control.Applicative.Ops
import Databrary.Model.Tag.Types

validTag :: Regex.Regex
validTag = Regex.makeRegex
  "^[a-z][-a-z ]+[a-z]$"

validateTag :: BS.ByteString -> Maybe TagName
validateTag t = Regex.matchTest validTag tt ?> TagName tt where
  tt = BSC.unwords $ BSC.words t
