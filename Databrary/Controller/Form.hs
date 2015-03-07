{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Form
  ( FormData
  , runFormWith
  , runForm
  , blankForm

  , emailTextForm
  , passwordForm
  ) where

import qualified Data.ByteString as BS
import Control.Applicative ((<$>))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask)
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Network.HTTP.Types (badRequest400)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Regex.Posix as Regex

import Databrary.Action
import Databrary.Action.Types
import Databrary.Action.App (MonadAppAction)
import Databrary.Web.Form (getFormData, FormData)
import Databrary.Web.Form.Deform
import Databrary.Web.Form.View (runFormView, blankFormView)
import Databrary.Web.Form.Errors (FormErrors)
import Databrary.View.Form (FormHtml)

apiFormErrors :: MonadAction c m => FormErrors -> m Response
apiFormErrors = returnResponse badRequest400 [] . JSON.toJSON

htmlFormErrors :: MonadAction c m => (FormErrors -> Html.Html) -> FormErrors -> m Response
htmlFormErrors f = returnResponse badRequest400 [] . f

handleForm :: (MonadAction c m, MonadIO m) => (FormErrors -> m Response) -> Either FormErrors a -> m a
handleForm re = either (result <=< re) return

handleFormErrors :: (MonadAction c m, MonadIO m) => Maybe (FormErrors -> Html.Html) -> Either FormErrors a -> m a
handleFormErrors = handleForm . maybe apiFormErrors htmlFormErrors

runFormWith :: (MonadAppAction q m, MonadIO m) => FormData -> Maybe (q -> FormHtml) -> DeformT m a -> m a
runFormWith fd mf fa = do
  req <- ask
  let fv hv = runFormView (hv req) fd
  handleFormErrors (fv <$> mf) =<< runDeform fa fd

runForm :: (MonadAppAction q m, MonadIO m) => Maybe (q -> FormHtml) -> DeformT m a -> m a
runForm mf fa = do
  (fd, _) <- getFormData []
  runFormWith fd mf fa

blankForm :: ActionData q => (q -> FormHtml) -> Action q
blankForm hv =
  okResponse [] . blankFormView . hv =<< ask

emailRegex :: Regex.Regex
emailRegex = Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt
  ("^[-a-z0-9!#$%&'*+/=?^_`{|}~.]*@[a-z0-9][a-z0-9\\.-]*[a-z0-9]\\.[a-z][a-z\\.]*[a-z]$" :: String)

emailTextForm :: (Functor m, Monad m) => DeformT m T.Text
emailTextForm = deformRegex "Invalid email address" emailRegex

passwordForm :: (Functor m, Monad m) => DeformT m BS.ByteString
passwordForm = do
  p <- "once" .:> (deformCheck "Password too short. Must be 7 characters." ((7 <=) . BS.length) =<< deform)
  _ <- "again" .:> (deformCheck "Passwords do not match." (p ==) =<< deform)
  return p
