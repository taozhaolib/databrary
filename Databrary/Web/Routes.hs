{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Routes
  ( generateRoutesJS
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import Data.Monoid ((<>))
import System.IO (hPutStr, withFile, IOMode(WriteMode))

import Databrary.JSON (quoteByteString)
import Databrary.Store
import Databrary.Model.Id.Types
import Databrary.HTTP.Path.JS
import Databrary.HTTP.Route
import Databrary.Action.Route
import Databrary.Web.Files

import {-# SOURCE #-} Databrary.Controller.VolumeAccess

jsRoute :: BS.ByteString -> Route r a -> a -> B.Builder
jsRoute n r v = quoteByteString '"' n
  <> B.string7 ":{method:" <> quoteByteString '"' (routeMethod r)
  <> B.string7 ",route:" <> jsPath (routePath r) v <> B.string7 "},"

generateRoutesJS :: RawFilePath -> IO Bool
generateRoutesJS f = webRegenerate undefined f Nothing $ \wf ->
  withFile (unRawFilePath wf) WriteMode $ \h -> do
    hPutStr h "app.constant('routeData',{"
    B.hPutBuilder h $ jsRoute "foo" postVolumeAccess (JSON, (Id 0, VolumeAccessTarget (Id 0)))
    hPutStr h "});"
