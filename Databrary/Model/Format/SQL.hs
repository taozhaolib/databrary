{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Format.SQL
  ( formatRow
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Databrary.Model.SQL
import Databrary.Model.Id.Types
import Databrary.Model.Format.Types

makeFormat :: Id Format -> BS.ByteString -> [Maybe String] -> T.Text -> Format
makeFormat i m e n = Format i m (map (fromMaybe "NULL format.extension") e) n

formatRow :: Selector
formatRow = selectColumns 'makeFormat "format" ["id", "mimetype", "extension", "name"]
