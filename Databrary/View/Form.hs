{-# LANGUAGE OverloadedStrings #-}
module Databrary.View.Form
  ( inputText
  , inputPassword
  , inputCheckbox
  , inputSelect
  , inputEnum
  , renderForm
  ) where

import Data.Monoid (mempty)
import qualified Data.Text as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Digestive as F

import Databrary.Model.Enum
import Databrary.View.Html
import Databrary.Action

absoluteRef :: T.Text -> F.View v -> T.Text
absoluteRef ref F.View{ F.viewContext = [] } = ref
absoluteRef ref F.View{ F.viewContext = ctx } =
  T.concat $ (concatMap (\x -> [x,"."]) ctx) ++ [ref]

refId :: T.Text -> F.View v -> H.AttributeValue
refId ref view = H.toValue $ absoluteRef ref view

label :: T.Text -> F.View v -> H.Html -> H.Html
label ref view = H.label
  H.! HA.for ref'
  where ref' = refId ref view

inputText :: T.Text -> F.View v -> H.Html
inputText ref view = H.input
  H.! HA.type_ "text"
  H.! HA.id    ref'
  H.! HA.name  ref'
  H.! HA.value (H.toValue $ F.fieldInputText ref view)
  where ref' = refId ref view

inputPassword :: T.Text -> F.View v -> H.Html
inputPassword ref view = H.input
  H.! HA.type_ "password"
  H.! HA.id    ref'
  H.! HA.name  ref'
  H.! HA.value (H.toValue $ F.fieldInputText ref view)
  where ref' = refId ref view

inputCheckbox :: T.Text -> F.View v -> H.Html
inputCheckbox ref view = H.input
  H.! HA.type_ "checkbox"
  H.! HA.id    ref'
  H.! HA.name  ref'
  H.!? (F.fieldInputBool ref view, HA.checked "checked")
  where ref'     = refId ref view

inputSelect :: (H.ToValue a, H.ToMarkup b) => [(a, b, Bool)] -> T.Text -> F.View v -> H.Html
inputSelect choices ref view = H.select
  H.! HA.id   ref'
  H.! HA.name ref'
  $ mapM_ (\(i, c, sel) -> H.option
    H.!  HA.value (H.toValue i)
    H.!? (sel, HA.selected "selected")
    $ H.toHtml c) choices
  where ref' = refId ref view

inputEnum :: DBEnum a => a -> T.Text -> F.View v -> H.Html
inputEnum a = inputSelect $ map (\(x, v) -> (fromEnum x, v, x == a)) $ pgEnumValues

errorList :: [H.Html] -> H.Html
errorList [] = mempty
errorList errs =
  H.ul H.! HA.class_ "error-list" $ mapM_
    (H.li H.! HA.class_ "error") errs

errorLists :: [(T.Text, H.Html)] -> H.Html
errorLists [] = mempty
errorLists errs =
  H.dl H.! HA.class_ "error-list" $ mapM_ (\(n,e) -> do
    H.dt $ H.toHtml n
    H.dd H.! HA.class_ "error" $ e) errs

renderForm :: AppRAction -> [(T.Text, T.Text -> F.View H.Html -> H.Html)] -> F.View H.Html -> H.Html
renderForm act inputs form = H.form
  H.! HA.method  (H.unsafeByteStringValue $ actionMethod act)
  H.! HA.enctype (H.toValue $ show $ F.viewEncType form)
  H.! HA.action  (byteStringValue $ actionRoute act) $ do
    mapM_ (\(name, input) -> do
      label name form (H.toHtml name)
      input name form
      errorList $ F.errors name form
      H.br)
      inputs
    H.input
      H.! HA.type_ "submit"
    errorLists [ (n, e) 
      | (p, e) <- F.viewErrors form
      , let n = F.fromPath p
      , n `notElem` names
      ]
  where names = map fst inputs
