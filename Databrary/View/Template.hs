{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Template
  ( 
  ) where

import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Digestive as F
import qualified Network.Wai as Wai

import Databrary.Action
import Databrary.View.Html

header :: Wai.Request -> H.Html
header req = do
  when (Wai.requestMethad req == methodGet && hasjs) $
    H.link
      H.! HA.rel "canonical"
      H.! HA.href (builderValue $ encodePath (Wai.pathInfo req) nojsq)
  H.link
    H.! HA.rel "shortcut icon"
    H.! HA.href "/public/icons/favicon.png"
  H.link
    H.! HA.rel "start"
    H.! HA.href "/"
  where
  (hasjs, nojsq) = nojs $ Wai.queryString req
  nojs [] = (False, [])
  nojs (("js",_):q) = (True, snd $ nojs q)
  nojs (x:q) = second (x:) $ nojs q

footer :: H.Html
footer = mempty

htmlTemplate :: AuthRequest -> Maybe T.Text -> H.Html -> H.Html
htmlTemplate req title body = H.docTypeHtml $ do
  H.head $ do
    header (see req)
    H.title $ do
      mapM_ (>> " || ") title
      "Databrary"
  H.body $ do
    H.section
      H.! HA.id "toolbar"
      H.! HA.class_ "toolbar" $ do
  <section id="toolbar" class="toolbar cf">
    <a href="/">Databrary</a>
    </h1>

          @request match {
            case request : SiteRequest.Auth[_] => {
              <a href="@routes.PartyHtml.profile(Some(false))">
                <span class="username">@request.identity.name</span>
              </a>

              @if(request.access.isAdmin) {
                @if(request.superuser) {
                  @helper.form(routes.LoginHtml.superuserOff) {
                    <button type="submit" class="link toolbar-superuser on">SU</button>
                  }
                } else {
                  @defining(new LoginController.SuperuserForm()(request)) { form =>
                    @widget.tag.form(form) {
                      @defining(helper.FieldConstructor(widget.tag.rawFieldConstructor.f)) { implicit fieldConstructor =>
                        @widget.tag.inputPassword(form.auth(), 'class -> "mini toolbar-static-input password")
                        <button type="submit" class="link toolbar-superuser off">SU</button>
                      }
                    }(request)
                  }
                }
              }

              @widget.tag.form(LoginController.LogoutForm) {
                <button type="submit" class="link">@Messages("toolbar.logout")</button>
              }(request)
            }

            case _ => {
              <a href="@routes.LoginHtml.view(Some(false))">@Messages("toolbar.login")</a>
            }
          }
  </section>

  @body
  @footer()
</body>
</html>
