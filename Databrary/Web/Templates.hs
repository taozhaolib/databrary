{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Templates
  ( generateTemplatesJS
  ) where

import Control.Monad (guard, unless, forM_)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isSpace)
import Data.Monoid ((<>))
import System.IO (withFile, IOMode(ReadMode, WriteMode), hPutStr, hIsEOF)

import qualified Databrary.JSON as JSON
import Databrary.Store
import Databrary.Web.Files

processTemplate :: RawFilePath -> (BS.ByteString -> IO ()) -> IO ()
processTemplate f g = withFile (unRawFilePath f) ReadMode go where
  go h = do
    e <- hIsEOF h
    unless e $ do
      l <- BS.hGetLine h
      g $ BSC.dropWhile isSpace l
      go h

generateTemplatesJS :: WebGenerator
generateTemplatesJS f t = do
  tl <- lift $ findWebFiles ".html"
  guard (not $ null tl)
  tt <- mapM webFileTime tl
  lift $ webRegenerate (maximum tt) f t $ \wf -> do
    withFile (webFileAbs wf) WriteMode $ \h -> do
      hPutStr h "app.run(['$templateCache',function(t){"
      forM_ tl $ \tf -> do
        BSB.hPutBuilder h $ BSB.string7 "t.put(" <> JSON.quoteByteString q (webFileRelRaw tf) <> BSB.char7 ',' <> BSB.char7 q
        processTemplate (webFileAbsRaw tf) $
          BSB.hPutBuilder h . JSON.escapeByteString q
        hPutStr h $ q : ");"
      hPutStr h "}]);"
  where q = '\''
