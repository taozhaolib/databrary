module Databrary.View.Html
  ( lazyByteStringValue
  , byteStringValue
  , builderValue
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Text.Blaze as Markup

import Blaze.ByteString.Builder.Html.Word (fromHtmlEscapedByteString, fromHtmlEscapedLazyByteString)

lazyByteStringValue :: BSL.ByteString -> Markup.AttributeValue
lazyByteStringValue = Markup.unsafeLazyByteStringValue . Blaze.toLazyByteString . fromHtmlEscapedLazyByteString

byteStringValue :: BS.ByteString -> Markup.AttributeValue
byteStringValue = Markup.unsafeByteStringValue . Blaze.toByteString . fromHtmlEscapedByteString

builderValue :: Blaze.Builder -> Markup.AttributeValue
builderValue = lazyByteStringValue . Blaze.toLazyByteString
