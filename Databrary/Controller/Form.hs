{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Form
  ( runForm

  , emailTextForm
  , enumForm

{-
  , optionalEnumForm
  , checkReadForm
  , optionalCheckReadForm
  -}
  ) where

import Control.Applicative (Applicative, (<$))
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson.Types as JSON
import qualified Data.Foldable as Fold
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Types (Status, ok200, badRequest400)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Regex.Posix as Regex

import Control.Has (view)
import Databrary.Action
import Databrary.Model.Kind
import Databrary.Model.Enum
import Databrary.Web.Form

putJson :: JSON.Value -> FormPath -> T.Text -> JSON.Value
putJson JSON.Null [] v = JSON.String v
putJson (JSON.Object o) [] v = JSON.Object $ HM.insertWith (\_ x -> putJson x [] v) "" (JSON.String v) o
putJson (JSON.Array a) [] v = JSON.Array $ V.snoc a $ JSON.String v
putJson o [] v = JSON.Array $ V.fromListN 2 [o, JSON.String v]
putJson (JSON.Object o) (k:p) v = JSON.Object $ HM.insertWith (\_ x -> putJson x p v) (view k) (putJson JSON.Null p v) o
putJson JSON.Null (k:p) v = JSON.Object $ HM.singleton (view k) (putJson JSON.Null p v)
putJson o (k:p) v = JSON.Object $ HM.fromList [("", o), (view k, putJson JSON.Null p v)]

jsonFormErrors :: FormErrors -> JSON.Value
jsonFormErrors = foldl' (uncurry . putJson) JSON.emptyObject

apiFormErrors :: ActionM c m => FormErrors -> m Response
apiFormErrors = returnResponse badRequest400 [] . jsonFormErrors

htmlFormErrors :: ActionM c m => (FormErrors -> Html.Html) -> FormErrors -> m Response
htmlFormErrors f = returnResponse badRequest400 [] . f

handleForm :: (ActionM c m, MonadIO m) => (FormErrors -> m Response) -> Either FormErrors a -> m a
handleForm re = either (result <=< re) return

handleFormErrors :: (ActionM c m, MonadIO m) => Maybe (FormErrors -> Html.Html) -> Either FormErrors a -> m a
handleFormErrors = handleForm . maybe apiFormErrors htmlFormErrors

runForm :: (ActionM q m, MonadIO m) => Maybe (FormData -> FormErrors -> Html.Html) -> DeformT m a -> m a
runForm mf fa = do
  fd <- getFormData
  handleFormErrors (fmap ($ fd) mf) =<< runDeform fa fd


emailRegex :: Regex.Regex
emailRegex = Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt
  ("^[-a-z0-9!#$%&'*+/=?^_`{|}~.]*@[a-z0-9][a-z0-9\\.-]*[a-z0-9]\\.[a-z][a-z\\.]*[a-z]$" :: String)

emailTextForm :: (Functor m, Monad m) => DeformT m T.Text
emailTextForm = deformRegex "Invalid email address" emailRegex

enumForm :: forall a m . (Functor m, Monad m, DBEnum a) => DeformT m a
enumForm = maybe (minBound <$ deformError ("Invalid " <> kindOf (undefined :: a))) return . readDBEnum =<< deform

{-
checkReadForm :: (Monad m, Read a, Show a) => v -> (a -> Bool) -> Maybe a -> Form.Form v m a
checkReadForm e c = Form.check e c . Form.stringRead e

optionalCheckReadForm :: (Monad m, Read a, Show a) => v -> (a -> Bool) -> Maybe a -> Form.Form v m (Maybe a)
optionalCheckReadForm e c = Form.check e (Fold.all c) . Form.optionalStringRead e
-}
