{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Messages
  ( generateMessagesJS
  ) where

import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString.Builder as BSB
import qualified Data.Configurator as C
import System.IO (withFile, IOMode(WriteMode), hPutStr)

import Paths_databrary (getDataFileName)
import qualified Databrary.JSON as JSON
import Databrary.Store
import Databrary.Model.Time
import Databrary.Web.Files

generateMessagesJS :: RawFilePath -> Maybe Timestamp -> IO Bool
generateMessagesJS f t = do
  -- it'd be nice to use Service.Messages here but there's no good way
  mf <- getDataFileName "messages.conf"
  fileInfo (rawFilePath mf) >>= maybe (return False) (\(_, mt) ->
    webRegenerate mt f t $ \wf -> do
      ml <- C.getMap =<< C.load [C.Optional mf]
      withFile (unRawFilePath wf) WriteMode $ \h -> do
        hPutStr h "app.constant('messagesData',"
        BSB.hPutBuilder h $ JSON.encodeToByteStringBuilder $ JSON.toJSON ml
        hPutStr h ");")
