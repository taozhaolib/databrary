{-# LANGUAGE OverloadedStrings, ConstraintKinds #-}
module Databrary.Web.Form 
  ( runForm
  , emailTextForm
  ) where

import Control.Arrow (second)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Network.HTTP.Types (badRequest400)
import qualified Network.Wai as Wai
import qualified Text.Digestive as Form
import qualified Text.Regex.Posix as Regex

import Control.Has (peeks)
import Databrary.Web.Parse
import Databrary.Action

lookupJson :: Form.Path -> JSON.Value -> Maybe T.Text
lookupJson [] (JSON.String s) = Just s
lookupJson [] JSON.Null = Nothing
lookupJson [] o = Just $ T.pack $ show o
lookupJson (h:p) v = lookupJson p =<< idx v where
  idx (JSON.Object o) = HM.lookup h o
  idx (JSON.Array a) = (a V.!?) =<< tr (TR.decimal h)
  idx _ = Nothing
  tr (Right (x, "")) = Just x
  tr _ = Nothing

putJson :: JSON.Value -> Form.Path -> T.Text -> JSON.Value
putJson JSON.Null [] v = JSON.String v
putJson (JSON.Object o) [] v = JSON.Object $ HM.insertWith (\_ x -> putJson x [] v) "" (JSON.String v) o
putJson (JSON.Array a) [] v = JSON.Array $ V.snoc a $ JSON.String v
putJson o [] v = JSON.Array $ V.fromListN 2 [o, JSON.String v]
putJson (JSON.Object o) (h:p) v = JSON.Object $ HM.insertWith (\_ x -> putJson x p v) h (putJson JSON.Null p v) o
putJson JSON.Null (h:p) v = JSON.Object $ HM.singleton h (putJson JSON.Null p v)
putJson o (h:p) v = JSON.Object $ HM.fromList [("", o), (h, putJson JSON.Null p v)]

postForm :: (MonadIO m, RequestM c m) => T.Text -> Form.Form v m a -> m (Form.View v, Maybe a)
postForm name form = do
  q <- peeks $ map (second $ fromMaybe "") . Wai.queryString
  c <- parseRequestContent
  let (p, j) = case c of
        ContentForm pp _ -> (pp, Nothing)
        ContentJSON jj -> ([], Just jj)
        _ -> ([], Nothing)
      qm = HM.fromListWith (++) $ map (second return) $ q <> p
      env i =
        map (Form.TextInput . TE.decodeUtf8) (HM.lookupDefault [] (TE.encodeUtf8 $ Form.fromPath i) qm)
        <> maybe [] (return . Form.TextInput) (lookupJson i =<< j)
        -- <> fmap FileInput (lookup p f')
  Form.postForm name form (const $ return $ return . env)

jsonFormErrors :: Form.View T.Text -> JSON.Value
jsonFormErrors = foldl' (uncurry . putJson) JSON.Null . Form.viewErrors

runForm :: (MonadIO m, RequestM c m) => T.Text -> Form.Form T.Text m a -> m a
runForm name form = do
  (v, f) <- postForm name form
  case f of
    Nothing -> liftIO $ resultWith $
      jsonResult badRequest400 $ jsonFormErrors v
    Just x -> return x

emailRegex :: Regex.Regex
emailRegex = Regex.makeRegexOpts Regex.compIgnoreCase Regex.blankExecOpt
  ("^[-a-z0-9!#$%&'*+/=?^_`{|}~.]*@[a-z0-9][\\w\\.-]*[a-z0-9]\\.[a-z][a-z\\.]*[a-z]$" :: String)

emailTextForm :: Monad m => Form.Form T.Text m T.Text
emailTextForm = Form.check "Invalid email address" (Regex.matchTest emailRegex . T.unpack) (Form.text Nothing)
