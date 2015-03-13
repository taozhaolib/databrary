module Databrary.View.Html
  ( lazyByteStringHtml
  , byteStringHtml
  , builderHtml
  , lazyByteStringValue
  , byteStringValue
  , builderValue
  , actionLink
  , actionForm
  , (!?)
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Text.Blaze.Internal as Markup
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Blaze.ByteString.Builder.Html.Word (fromHtmlEscapedByteString, fromHtmlEscapedLazyByteString)
import Databrary.Action

lazyByteStringHtml :: BSL.ByteString -> H.Markup
lazyByteStringHtml = H.unsafeLazyByteString . Blaze.toLazyByteString . fromHtmlEscapedLazyByteString

byteStringHtml :: BS.ByteString -> H.Markup
byteStringHtml = H.unsafeByteString . Blaze.toByteString . fromHtmlEscapedByteString

builderHtml :: Blaze.Builder -> H.Markup
builderHtml = lazyByteStringHtml . Blaze.toLazyByteString

lazyByteStringValue :: BSL.ByteString -> H.AttributeValue
lazyByteStringValue = H.unsafeLazyByteStringValue . Blaze.toLazyByteString . fromHtmlEscapedLazyByteString

byteStringValue :: BS.ByteString -> H.AttributeValue
byteStringValue = H.unsafeByteStringValue . Blaze.toByteString . fromHtmlEscapedByteString

builderValue :: Blaze.Builder -> H.AttributeValue
builderValue = lazyByteStringValue . Blaze.toLazyByteString

actionLink :: RouteAction q -> H.Attribute
actionLink r = HA.href $ byteStringValue $ actionURL r Nothing

actionForm :: RouteAction q -> H.Html -> H.Html
actionForm RouteAction{ actionMethod = g, actionRoute = r } = H.form
  H.! HA.method (H.unsafeByteStringValue g)
  -- H.! HA.enctype (H.toValue $ show $ F.viewEncType form)
  H.! HA.action (byteStringValue r)

(!?) :: Markup.Attributable h => h -> Maybe H.Attribute -> h
h !? Nothing = h
h !? (Just a) = h H.! a
