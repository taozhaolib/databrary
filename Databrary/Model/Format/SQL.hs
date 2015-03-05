{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Format.SQL
  ( formatRow
  ) where

import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Databrary.Model.SQL.Select
import Databrary.Model.Id.Types
import Databrary.Model.Format.Types

makeFormat :: Id Format -> BS.ByteString -> [Maybe BS.ByteString] -> T.Text -> Format
makeFormat i m e n = Format i m (map (fromMaybe (error "NULL format.extension")) e) n

formatRow :: Selector -- Format
formatRow = selectColumns 'makeFormat "format" ["id", "mimetype", "extension", "name"]
