{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Form
  ( FormData
  , runForm

  , emailTextForm
  ) where

import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Network.HTTP.Types (badRequest400)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Regex.Posix as Regex

import Databrary.Action
import Databrary.Web.Form (getFormData, FormData)
import Databrary.Web.Form.Deform (DeformT, runDeform, deformRegex)
import Databrary.Web.Form.View (runFormView)
import Databrary.Web.Form.Errors (FormErrors)
import Databrary.View.Form (FormHtml)

apiFormErrors :: ActionM c m => FormErrors -> m Response
apiFormErrors = returnResponse badRequest400 [] . JSON.toJSON

htmlFormErrors :: ActionM c m => (FormErrors -> Html.Html) -> FormErrors -> m Response
htmlFormErrors f = returnResponse badRequest400 [] . f

handleForm :: (ActionM c m, MonadIO m) => (FormErrors -> m Response) -> Either FormErrors a -> m a
handleForm re = either (result <=< re) return

handleFormErrors :: (ActionM c m, MonadIO m) => Maybe (FormErrors -> Html.Html) -> Either FormErrors a -> m a
handleFormErrors = handleForm . maybe apiFormErrors htmlFormErrors

runForm :: (ActionM q m, MonadIO m) => Maybe FormHtml -> DeformT m a -> m a
runForm mf fa = do
  fd <- getFormData
  handleFormErrors (fmap (\hv -> runFormView hv fd) mf) =<< runDeform fa fd


emailRegex :: Regex.Regex
emailRegex = Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt
  ("^[-a-z0-9!#$%&'*+/=?^_`{|}~.]*@[a-z0-9][a-z0-9\\.-]*[a-z0-9]\\.[a-z][a-z\\.]*[a-z]$" :: String)

emailTextForm :: (Functor m, Monad m) => DeformT m T.Text
emailTextForm = deformRegex "Invalid email address" emailRegex
