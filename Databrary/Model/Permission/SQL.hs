{-# LANGUAGE TemplateHaskell #-}
module Databrary.Model.Permission.SQL
  ( accessRow
  ) where

import Databrary.Model.SQL
import Databrary.Model.Permission.Types

accessRow :: String -- ^ Table name
  -> Selector -- ^ 'Access'
accessRow table = selectColumns 'Access table ["site", "member"]

