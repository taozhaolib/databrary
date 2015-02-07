{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Form
  ( displayForm
  , emailTextForm
  , optionalEnumForm
  , checkReadForm
  , optionalCheckReadForm
  ) where

import qualified Data.Aeson.Types as JSON
import qualified Data.Foldable as Fold
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.HTTP.Types (ok200, badRequest400)
import qualified Text.Blaze.Html5 as Html
import qualified Text.Digestive as Form
import qualified Text.Regex.Posix as Regex

import Databrary.Action
import Databrary.Kind
import Databrary.Enum

putJson :: JSON.Value -> Form.Path -> T.Text -> JSON.Value
putJson JSON.Null [] v = JSON.String v
putJson (JSON.Object o) [] v = JSON.Object $ HM.insertWith (\_ x -> putJson x [] v) "" (JSON.String v) o
putJson (JSON.Array a) [] v = JSON.Array $ V.snoc a $ JSON.String v
putJson o [] v = JSON.Array $ V.fromListN 2 [o, JSON.String v]
putJson (JSON.Object o) (h:p) v = JSON.Object $ HM.insertWith (\_ x -> putJson x p v) h (putJson JSON.Null p v) o
putJson JSON.Null (h:p) v = JSON.Object $ HM.singleton h (putJson JSON.Null p v)
putJson o (h:p) v = JSON.Object $ HM.fromList [("", o), (h, putJson JSON.Null p v)]

jsonFormErrors :: Form.View T.Text -> JSON.Value
jsonFormErrors = foldl' (uncurry . putJson) JSON.emptyObject . Form.viewErrors

displayForm :: ActionM c m => Bool -> (Form.View Html.Html -> Html.Html) -> Form.View T.Text -> m Response
displayForm api view form =
  if api
    then returnResponse s h $ jsonFormErrors form
    else returnResponse s h $ view $ fmap Html.toHtml form
  where
  s = if null $ Form.viewErrors form
    then ok200 else badRequest400
  h = []


{-
class Formable a where
  formField :: Formlet T.Text m a
  formInput :: a -> T.Text -> F.View v -> H.Html
-}

emailRegex :: Regex.Regex
emailRegex = Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt
  ("^[-a-z0-9!#$%&'*+/=?^_`{|}~.]*@[a-z0-9][a-z0-9\\.-]*[a-z0-9]\\.[a-z][a-z\\.]*[a-z]$" :: String)

emailTextForm :: Monad m => Maybe T.Text -> Form.Form T.Text m T.Text
emailTextForm = Form.check "Invalid email address" (Regex.matchTest emailRegex . T.unpack) . Form.text

optionalEnumForm :: forall a m . (DBEnum a, Monad m) => Maybe a -> Form.Form T.Text m (Maybe a)
optionalEnumForm = Form.validateOptional
  (maybe (Form.Error $ "Invalid " <> kindOf (undefined :: a)) Form.Success . readDBEnum)
  . Form.optionalString . fmap (show . fromEnum)

checkReadForm :: (Monad m, Read a, Show a) => v -> (a -> Bool) -> Maybe a -> Form.Form v m a
checkReadForm e c = Form.check e c . Form.stringRead e

optionalCheckReadForm :: (Monad m, Read a, Show a) => v -> (a -> Bool) -> Maybe a -> Form.Form v m (Maybe a)
optionalCheckReadForm e c = Form.check e (Fold.all c) . Form.optionalStringRead e
