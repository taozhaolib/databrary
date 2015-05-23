{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Messages
  ( generateMessagesJS
  ) where

import Control.Monad.Trans.Class (lift)
import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString.Builder as BSB
import qualified Data.Configurator as C
import System.IO (withBinaryFile, IOMode(WriteMode), hPutStr)

import Paths_databrary (getDataFileName)
import qualified Databrary.JSON as JSON
import Databrary.Store
import Databrary.Web.Files

generateMessagesJS :: WebGenerator
generateMessagesJS f t = do
  -- it'd be nice to use Service.Messages here but there's no good way
  mf <- lift $ getDataFileName "messages.conf"
  mt <- fileTime (rawFilePath mf)
  lift $ webRegenerate mt f t $ \wf -> do
    ml <- C.getMap =<< C.load [C.Optional mf]
    withBinaryFile (webFileAbs wf) WriteMode $ \h -> do
      hPutStr h "app.constant('messagesData',"
      BSB.hPutBuilder h $ JSON.encodeToByteStringBuilder $ JSON.toJSON ml
      hPutStr h ");"
