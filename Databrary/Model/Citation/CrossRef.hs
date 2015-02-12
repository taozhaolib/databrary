{-# LANGUAGE OverloadedStrings #-}
module Databrary.Model.Citation.CrossRef
  ( lookupCitation
  ) where

import Control.Applicative ((<$>), (<*>), (<|>), (<$))
import Control.Exception (handle)
import Control.Monad (guard)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import Data.Maybe (fromJust)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (hAccept, hContentType, ok200)
import qualified Network.HTTP.Client as HC
import qualified Network.URI as URI

import Control.Has (MonadHas, peek)
import qualified Databrary.JSON as JSON
import Databrary.URL
import Databrary.Model.Citation.Types

crossRefUrl :: HC.Request
crossRefUrl = fromJust $ HC.parseUrl "http://data.crossref.org/"

crossRefReq :: String -> BS.ByteString -> HC.Request
crossRefReq h a = crossRefUrl
  { HC.path = BSC.pack $ '/' : URI.escapeURIString URI.isUnescapedInURIComponent h
  , HC.requestHeaders = [(hAccept, a)]
  }

uriHDL :: URI.URI -> Maybe String
uriHDL u
  | URI.uriScheme u == "hdl:" = Just $ URI.uriPath u ++ URI.uriQuery u
  | otherwise = Nothing

parseCitation :: JSON.Value -> JSON.Parser Citation
parseCitation = JSON.withObject "citation" $ \o ->
  Citation
    <$> o JSON..:? "head" JSON..!= ""
    <*> o JSON..:? "title"
    <*> (Just <$> (o JSON..: "DOI" >>= parseDOI))
    <*> ((o JSON..:? "issued" >>=? (JSON..:? "date-parts") >>=? (JSON..!? 0) >>=? (JSON..!? 0)) <|> return Nothing)
  where
  parseDOI d = hdlURL d <$ guard (validHDL d)
  m >>=? f = m >>= maybe (return Nothing) f

lookupCitation :: (MonadHas HC.Manager c m, MonadIO m) => URI.URI -> m (Maybe Citation)
lookupCitation uri = runMaybeT $ do
  hdl <- may $ uriHDL uri
  hcm <- peek
  let req a f = liftIO $ handle (\(_ :: HC.HttpException) -> return Nothing) $
        HC.withResponse (crossRefReq hdl a) hcm $ \res ->
          if HC.responseStatus res == ok200 && lookup hContentType (HC.responseHeaders res) == Just (BSC.takeWhile (';' /=) a)
            then f $ HC.responseBody res
            else return Nothing
  j <- MaybeT $ req "application/vnd.citationstyles.csl+json" $ \rb ->
    P.maybeResult <$> P.parseWith rb JSON.json BS.empty
  cite <- may $ JSON.parseMaybe parseCitation j
  -- empirically this is UTF-8, but does not say so:
  bib <- lift $ req "text/x-bibliography;style=apa" $ fmap Just . HC.brConsume
  return $ maybe cite (\h -> cite{ citationHead = Fold.foldMap TE.decodeUtf8 h }) bib
  where
  may = MaybeT . return
