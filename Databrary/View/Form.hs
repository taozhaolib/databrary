{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Form
  ( FormHtml
  , field
  , inputText
  , inputPassword
  , inputCheckbox
  , inputSelect
  , inputEnum
  , renderForm
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (liftWith)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import Data.Maybe (isJust)
import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Text.Blaze.Internal as M
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA

import Control.Applicative.Ops
import Control.Has (peek, peeks)
import Databrary.Model.Enum
import Databrary.View.Html
import Databrary.Action
import Databrary.Web.Form
import Databrary.Web.Form.Errors
import Databrary.Web.Form.View

type FormHtmlM = FormViewT M.MarkupM
type FormHtml = FormHtmlM ()

pathId :: FormHtmlM H.AttributeValue
pathId = peeks byteStringValue

value :: FormHtmlM (Maybe BS.ByteString)
value = do
  val <- peek
  return $ case val of
    FormDatumNone -> Nothing
    FormDatumJSON _ -> Nothing -- that's weird
    FormDatumBS b -> BS.null b ?!> b

type Field = H.AttributeValue -> Maybe BS.ByteString -> H.Html

errorList :: [FormErrorMessage] -> H.Html
errorList [] = mempty
errorList err =
  H.ul H.! HA.class_ "error-list" $ mapM_
    ((H.li H.! HA.class_ "error") . H.toHtml) err

errorLists :: [(FormPath, FormErrorMessage)] -> H.Html
errorLists [] = mempty
errorLists err =
  H.dl H.! HA.class_ "error-list" $ mapM_ (\(p,e) -> do
    H.dt $ H.toHtml (formPathText p)
    H.dd H.! HA.class_ "error" $ H.toHtml e) err

label :: H.AttributeValue -> H.Html -> H.Html
label ref = H.label
  H.! HA.for ref

field :: T.Text -> Field -> FormHtml
field k sub = k .:> do
  ref <- pathId
  err <- formViewErrors
  val <- value
  lift $ label ref $ do
    sub ref val
    errorList err

inputText :: Field
inputText ref val = H.input
  H.! HA.type_ "text"
  H.! HA.id    ref
  H.! HA.name  ref
  $? (fmap (flip (H.!) . HA.value . byteStringValue) val)

inputPassword :: Field
inputPassword ref _ = H.input
  H.! HA.type_ "password"
  H.! HA.id    ref
  H.! HA.name  ref

inputCheckbox :: Field
inputCheckbox ref val = H.input
  H.! HA.type_ "checkbox"
  H.! HA.id    ref
  H.! HA.name  ref
  H.!? (isJust val, HA.checked "checked")

inputSelect :: H.ToMarkup b => [(BS.ByteString, b)] -> Field
inputSelect choices ref val = H.select
  H.! HA.id   ref
  H.! HA.name ref
  $ mapM_ (\(v, c) -> H.option
    H.!  HA.value (byteStringValue v)
    H.!? (Fold.any (v ==) val, HA.selected "selected")
    $ H.toHtml c) choices

inputEnum :: forall a . DBEnum a => a -> Field
inputEnum _ = inputSelect $ map (\(x, v) -> (BSC.pack $ show $ fromEnum (x :: a), v)) pgEnumValues

renderForm :: RouteAction q -> FormHtml -> FormHtml
renderForm act form =
  liftWith $ \run -> H.form
    H.! HA.method  (H.unsafeByteStringValue $ actionMethod act)
    -- H.! HA.enctype (H.toValue $ show $ F.viewEncType form)
    H.! HA.action  (byteStringValue $ actionRoute act)
    $ do
      ((), err) <- run form
      errorLists $ allFormErrors err
      H.input
        H.! HA.type_ "submit"
