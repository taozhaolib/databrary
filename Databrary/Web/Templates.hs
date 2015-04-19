{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Templates
  ( generateTemplatesJS
  ) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hPutStr)

import qualified Databrary.JSON as JSON
import Databrary.Store
import Databrary.Web.Files

processTemplate :: RawFilePath -> (BS.ByteString -> IO ()) -> IO ()
processTemplate f g = withFile (unRawFilePath f) ReadMode go where
  go h = do
    l <- BS.hGetLine h
    g $ BSC.dropWhile isSpace l
    go h

templateFiles :: IO [RawFilePath]
templateFiles = findWebFiles ".html"

generateTemplatesJS :: RawFilePath -> IO Bool
generateTemplatesJS f = do
  tl <- templateFiles
  if null tl then return False else do
  ti <- mapM (fmap (maybe (posixSecondsToUTCTime 0) snd) . fileInfo . webFile) tl
  webRegenerate (maximum ti) f $ \wf ->
    withFile (unRawFilePath wf) WriteMode $ \h -> do
      hPutStr h "app.run(['$templateCache',function(t){"
      forM_ tl $ \tf -> do
        BSB.hPutBuilder h $ BSB.string7 "t.put(" <> JSON.quoteByteString q tf <> BSB.char7 ',' <> BSB.char7 q
        processTemplate (webFile tf) $
          BSB.hPutBuilder h . JSON.escapeByteString q
        hPutStr h $ q : ");"
      hPutStr h "}]);"
  where q = '\''
