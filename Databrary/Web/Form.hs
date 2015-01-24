{-# LANGUAGE OverloadedStrings, ConstraintKinds #-}
module Databrary.Web.Form 
  ( postForm
  ) where

import Control.Arrow (second)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import qualified Network.Wai as Wai
import qualified Text.Digestive as Form

import Control.Has (peeks)
import Databrary.Web.Parse
import Databrary.Action.Types

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
