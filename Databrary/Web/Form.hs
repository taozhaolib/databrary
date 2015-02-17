{-# LANGUAGE OverloadedStrings, RecordWildCards, PatternGuards #-}
module Databrary.Web.Form 
  ( FormKey(..)
  , FormPath
  , FormData
  , Form(formPath)
  , initForm
  , subForm
  , subForms
  , getFormData
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.Wai as Wai

import Control.Has (Has(..), peeks)
import Databrary.Web.Parse
import Databrary.Action

data FormData = FormData
  { formDataQuery :: Map.Map BS.ByteString (Maybe BS.ByteString)
  , formDataPost :: Map.Map BS.ByteString BS.ByteString
  , formDataJSON :: Maybe JSON.Value
  }

data FormKey 
  = FormField !T.Text
  | FormIndex !Int

type FormPath = [FormKey]

formSubPath :: FormKey -> FormPath -> FormPath
formSubPath k p = p ++ [k]

formKeyBS :: FormKey -> BS.ByteString
formKeyBS (FormField t) = TE.encodeUtf8 t
formKeyBS (FormIndex i) = BSC.pack $ show i

dotsBS :: [BS.ByteString]  -> BS.ByteString
dotsBS = BS.intercalate (BSC.singleton '.')

dotBS :: BS.ByteString -> BS.ByteString -> BS.ByteString
dotBS a b
  | BS.null a = b
  | otherwise = dotsBS [a, b]

formSubBS :: FormKey -> BS.ByteString -> BS.ByteString
formSubBS k b = b `dotBS` formKeyBS k

_formPathBS :: FormPath -> BS.ByteString
_formPathBS = dotsBS . map formKeyBS

data Form = Form
  { formData :: FormData
  , formPath :: FormPath
  , formPrefix :: BS.ByteString
  , formJSON :: Maybe JSON.Value
  }

initForm :: FormData -> Form
initForm d = Form d [] "" (formDataJSON d)

formSubJSON :: FormKey -> JSON.Value -> Maybe JSON.Value
formSubJSON (FormField t) (JSON.Object o) = HM.lookup t o
formSubJSON (FormIndex i) (JSON.Object o) = HM.lookup (T.pack (show i)) o
formSubJSON (FormIndex i) (JSON.Array a) = a V.!? i
formSubJSON _ _ = Nothing

subForm :: FormKey -> Form -> Form
subForm key form = form
  { formPath = formSubPath key $ formPath form
  , formPrefix = formSubBS key $ formPrefix form
  , formJSON = formSubJSON key =<< formJSON form
  }

formEmpty :: Form -> Bool
formEmpty Form{ formJSON = Just _ } = False
formEmpty Form{ formPrefix = p, formData = FormData{..} } =
  me formDataQuery || me formDataPost where
  me = not . Fold.any (sk . fst) . Map.lookupGE p
  sk s = p `BS.isPrefixOf` s && (l == BS.length s || BSC.index s l == '.')
  l = BS.length p

subForms :: Form -> [Form]
subForms form = sf 0 where
  n | Just (JSON.Array v) <- formJSON form = V.length v
    | otherwise = 0
  sf i
    | i >= n && formEmpty el = []
    | otherwise = el : sf (succ i)
    where el = subForm (FormIndex i) form

data FormValue 
  = FormValueNone
  | FormValueBS !BS.ByteString
  | FormValueJSON !JSON.Value

instance Monoid FormValue where
  mempty = FormValueNone
  mappend FormValueNone x = x
  mappend x _ = x

jsonFormValue :: Form -> FormValue
jsonFormValue Form{ formJSON = j } = Fold.foldMap FormValueJSON j

queryFormValue :: Form -> FormValue
queryFormValue Form{ formData = FormData{ formDataQuery = m }, formPrefix = p } =
  Fold.foldMap (maybe (FormValueJSON JSON.Null) FormValueBS) $ Map.lookup p m

postFormValue :: Form -> FormValue
postFormValue Form{ formData = FormData{ formDataPost = m }, formPrefix = p } =
  Fold.foldMap FormValueBS $ Map.lookup p m

formValue :: Form -> FormValue
formValue form = postFormValue form <> jsonFormValue form <> queryFormValue form

instance Has FormValue Form where
  view = formValue

getFormData :: (ActionM c m, MonadIO m) => m FormData
getFormData = do
  f <- peeks $ FormData . Map.fromList . Wai.queryString
  c <- parseRequestContent
  return $ case c of
    ContentForm p _ -> f (Map.fromList p) Nothing
    ContentJSON j -> f Map.empty (Just j)
    _ -> f Map.empty Nothing
