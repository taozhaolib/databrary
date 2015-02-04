{-# LANGUAGE TypeSynonymInstances, DeriveDataTypeable, OverloadedStrings #-}
module Databrary.Action.Response
  ( Response
  , ResponseData(..)
  , Result(..)
  , result
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Typeable (Typeable)
import Network.HTTP.Types (ResponseHeaders, Status, hContentType)
import Network.Wai (Response, responseBuilder, responseLBS, StreamingBody, responseStream, FilePart(..), responseFile, responseStatus)
import qualified Text.Blaze.Html as Html
import qualified Text.Blaze.Html.Renderer.Utf8 as Html

class ResponseData r where
  response :: Status -> ResponseHeaders -> r -> Response

instance ResponseData (Status -> ResponseHeaders -> Response) where
  response s h r = r s h

instance ResponseData Blaze.Builder where
  response = responseBuilder

instance ResponseData BSL.ByteString where
  response = responseLBS

instance ResponseData StreamingBody where
  response = responseStream

instance ResponseData FilePath where
  response s h f = responseFile s h f Nothing

instance ResponseData (FilePath, FilePart) where
  response s h (f, p) = responseFile s h f (Just p)

instance ResponseData (FilePath, Maybe FilePart) where
  response s h (f, p) = responseFile s h f p

instance ResponseData T.Text where
  response s h =
    response s ((hContentType, "text/plain;charset=utf-8") : h) . Blaze.fromText

instance ResponseData TL.Text where
  response s h =
    response s ((hContentType, "text/plain;charset=utf-8") : h) . Blaze.fromLazyText

instance ResponseData JSON.Value where
  response s h =
    response s ((hContentType, "text/json;charset=utf-8") : h) . JSON.encode

instance ResponseData JSON.Object where
  response s h = response s h . JSON.Object

instance ResponseData Html.Html where
  response s h =
    response s ((hContentType, "text/html;charset=utf-8") : h) . Html.renderHtmlBuilder

newtype Result = Result { resultResponse :: Response } deriving (Typeable)
instance Show Result where
  showsPrec p (Result r) = showParen (p > 10)
    $ showString "Result " . showsPrec 11 (responseStatus r)
instance Exception Result

result :: MonadIO m => Response -> m a
result = liftIO . throwIO . Result
