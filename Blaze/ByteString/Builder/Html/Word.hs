{-# LANGUAGE OverloadedStrings #-}
module Blaze.ByteString.Builder.Html.Word
  ( writeHtmlEscapedWord
  , fromHtmlEscapedWord
  , fromHtmlEscapedWordList
  , fromHtmlEscapedByteString
  , fromHtmlEscapedLazyByteString
  , fromHtmlEscapedText
  , fromHtmlEscapedLazyText
  ) where

import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Internal.Write
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE

writeHtmlEscapedWord :: Word8 -> Write
writeHtmlEscapedWord w = boundedWrite 6 (io $ toEnum $ fromIntegral w) where
  io '<'  = getPoke $ writeByteString "&lt;"
  io '>'  = getPoke $ writeByteString "&gt;"
  io '&'  = getPoke $ writeByteString "&amp;"
  io '"'  = getPoke $ writeByteString "&quot;"
  io '\'' = getPoke $ writeByteString "&#39;"
  io _    = getPoke $ writeWord8 w

fromHtmlEscapedWord :: Word8 -> Builder
fromHtmlEscapedWord = fromWriteSingleton writeHtmlEscapedWord

fromHtmlEscapedWordList :: [Word8] -> Builder
fromHtmlEscapedWordList = fromWriteList writeHtmlEscapedWord

fromHtmlEscapedByteString :: BS.ByteString -> Builder
fromHtmlEscapedByteString = fromHtmlEscapedWordList . BS.unpack

fromHtmlEscapedLazyByteString :: BSL.ByteString -> Builder
fromHtmlEscapedLazyByteString = fromHtmlEscapedWordList . BSL.unpack

fromHtmlEscapedText :: T.Text -> Builder
fromHtmlEscapedText = fromHtmlEscapedByteString . TE.encodeUtf8

fromHtmlEscapedLazyText :: TL.Text -> Builder
fromHtmlEscapedLazyText = fromHtmlEscapedLazyByteString . TLE.encodeUtf8

