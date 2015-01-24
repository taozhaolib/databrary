{-# LANGUAGE OverloadedStrings, ConstraintKinds #-}
module Databrary.Web.Form 
  ( postForm
  ) where

import Control.Arrow (second)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import qualified Network.Wai as Wai
import qualified Network.Wai.Parse as WaiP
import qualified Text.Digestive as Form

import Control.Has (peek)
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
  rq <- peek
  (p, f) <- liftIO $ WaiP.parseRequestBody (dynamicBackEnd $ 4*1024*1024) rq
  let q = map (second $ fromMaybe "") $ Wai.queryString rq
      qm = HM.fromListWith (++) $ map (second return) $ q <> p
      (j, _f') = case (p, f) of
        ([], (_, WaiP.FileInfo{ WaiP.fileContent = DynamicJSON jj@(Just _) }) : f') -> (jj, f')
        _ -> (Nothing, f)
      env i =
        map (Form.TextInput . TE.decodeUtf8) (HM.lookupDefault [] (TE.encodeUtf8 $ Form.fromPath i) qm)
        <> maybe [] (return . Form.TextInput) (lookupJson i =<< j)
        -- <> fmap FileInput (lookup p f')
  Form.postForm name form (const $ return $ return . env)
