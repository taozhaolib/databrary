{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Angular
  ( htmlAngular
  ) where

import qualified Blaze.ByteString.Builder.ByteString as Blaze
import Control.Monad (forM_)
import qualified Data.Aeson.Encode as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid ((<>))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Network.HTTP.Types (encodePath)
import qualified Network.Wai as Wai

import Control.Applicative.Ops
import Control.Has (view)
import Databrary.Model.Identity
import Databrary.Action.Auth
import Databrary.Action
import Databrary.View.Html
import Databrary.View.Template

import Databrary.Controller.Static
import {-# SOURCE #-} Databrary.Controller.Angular

ngAttribute :: String -> H.AttributeValue -> H.Attribute
ngAttribute = H.customAttribute . H.stringTag . ("ng-" <>)

public :: [BS.ByteString] -> H.AttributeValue
public p = byteStringValue $ actionURL (staticPublicFile $ staticPath p) Nothing

htmlAngular :: AuthRequest -> H.Html
htmlAngular auth = H.docTypeHtml H.! ngAttribute "ng-app" "databraryModule" $ do
  H.head $ do
    nojsq <- htmlHeader req
    H.noscript $
      H.meta
        H.! HA.httpEquiv "Refresh"
        H.! HA.content (builderValue $ Blaze.fromByteString "0;url=" <> encodePath (Wai.pathInfo req) (("js",Just "0") : nojsq))
    H.meta
      H.! HA.httpEquiv "X-UA-Compatible"
      H.! HA.content "IE=edge"
    H.meta 
      H.! HA.name "viewport"
      H.! HA.content "width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
    H.title
      H.! ngAttribute "bind" "page.display.title + ' || Databrary'"
      $ "Databrary"
    forM_ [Just "114x114", Just "72x72", Nothing] $ \size ->
      H.link
        H.! HA.rel "apple-touch-icon-precomposed"
        H.! HA.href (public ["icons", "apple-touch-icon" <> maybe "" (BSC.cons '-') size <> ".png"])
        $? (flip (H.!) . HA.sizes . byteStringValue <$> size)
    H.link
      H.! HA.rel "stylesheet"
      H.! HA.href (public ["app.min.css"])
    H.script $ do
      H.preEscapedString "window.$play={user:"
      H.unsafeLazyByteString $ JSON.encode $ identityJSON (view auth)
      H.preEscapedString "};"
    forM_ [] $ \js ->
      H.script
        H.! HA.src (public js)
        $ return ()
    H.script
      H.! HA.src (byteStringValue $ actionURL angularConstants Nothing)
      $ return ()
    {-
<body flow-prevent-drop>
  <noscript>
    Our site works best with modern browsers (Firefox, Chrome, Safari &ge;6, IE &ge;10, and others) with Javascript enabled.
    You can also switch to the <a href="@display.url(nojs)">simple version</a> of this page.
  </noscript>
  <toolbar></toolbar>
  <main ng-view id="main" class="main" autoscroll ng-if="!page.display.error"></main>
  <errors></errors>
  @footer()
  <messages></messages>
  <tooltip ng-repeat="tooltip in page.tooltips.list track by tooltip.id" ng-if="tooltip.target"></tooltip>
  <div id="loading" class="loading" style="display:none" ng-show="page.display.loading">
    <div class="loading-animation">
      <div class="loading-spinner">
        <div class="loading-mask">
          <div class="loading-circle"></div>
        </div>
      </div>
      <div class="loading-text">[<span>loading</span>]</div>
    </div>
  </div>
  <script>
    document.getElementById('loading').style.display = 'block';
  </script>
</body>
</html>
  -}
  where req = view auth
