{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.CSV
  ( csvVolume
  ) where

import qualified Data.ByteString as BS
import Data.Monoid ((<>))
import Network.HTTP.Types (hContentType)

import Databrary.Store.Filename
import Databrary.Model.Id
import Databrary.Model.Permission
import Databrary.Model.Volume
import Databrary.HTTP
import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Controller.Paths
import Databrary.Controller.Volume

csvVolume :: AppRoute (Id Volume)
csvVolume = action GET (pathId </< "csv") $ \vi -> withAuth $ do
  v <- getVolume PermissionPUBLIC vi
  name <- volumeDownloadName v
  okResponse 
    [ (hContentType, "text/csv")
    , ("content-disposition", "attachment; filename=" <> quoteHTTP (makeFilename name <> ".csv"))
    ]
    BS.empty -- TODO
