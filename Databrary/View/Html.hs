module Databrary.View.Html
  ( lazyByteStringHtml
  , byteStringHtml
  , builderHtml
  , lazyByteStringValue
  , byteStringValue
  , builderValue
  , actionLink
  , actionForm
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import Network.HTTP.Types (methodGet)
import qualified Text.Blaze as Markup
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Blaze.ByteString.Builder.Html.Word (fromHtmlEscapedByteString, fromHtmlEscapedLazyByteString)
import Databrary.Action

lazyByteStringHtml :: BSL.ByteString -> Markup.Markup
lazyByteStringHtml = Markup.unsafeLazyByteString . Blaze.toLazyByteString . fromHtmlEscapedLazyByteString

byteStringHtml :: BS.ByteString -> Markup.Markup
byteStringHtml = Markup.unsafeByteString . Blaze.toByteString . fromHtmlEscapedByteString

builderHtml :: Blaze.Builder -> Markup.Markup
builderHtml = lazyByteStringHtml . Blaze.toLazyByteString

lazyByteStringValue :: BSL.ByteString -> Markup.AttributeValue
lazyByteStringValue = Markup.unsafeLazyByteStringValue . Blaze.toLazyByteString . fromHtmlEscapedLazyByteString

byteStringValue :: BS.ByteString -> Markup.AttributeValue
byteStringValue = Markup.unsafeByteStringValue . Blaze.toByteString . fromHtmlEscapedByteString

builderValue :: Blaze.Builder -> Markup.AttributeValue
builderValue = lazyByteStringValue . Blaze.toLazyByteString

actionLink :: RouteAction q -> H.Html -> H.Html
actionLink RouteAction{ actionMethod = g, actionRoute = r }
  | g == methodGet = H.a H.! HA.href (byteStringValue r)
  | otherwise = error ("actionLink: " ++ BSC.unpack g ++ " " ++ BSC.unpack r)

actionForm :: RouteAction q -> H.Html -> H.Html
actionForm RouteAction{ actionMethod = g, actionRoute = r } = H.form
  H.! HA.method (H.unsafeByteStringValue g)
  -- H.! HA.enctype (H.toValue $ show $ F.viewEncType form)
  H.! HA.action (byteStringValue r)
