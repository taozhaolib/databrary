{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Messages
  ( Messages
  , initMessages
  , getMessage
  , listMessages
  ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Short as BSS
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Databrary.Has (peeks, MonadHas)

newtype Messages = Messages { messagesMap :: HM.HashMap BSS.ShortByteString T.Text }

type MonadMessages c m = MonadHas Messages c m

initMessages :: C.Config -> IO Messages
initMessages c = Messages .
  HM.fromList . mapMaybe f . HM.toList <$> C.getMap c where
  f (k, C.String v) | Just m <- T.stripPrefix "message." k = Just (BSS.toShort $ TE.encodeUtf8 m, v)
  f _ = Nothing

getMessage :: MonadMessages c m => BSS.ShortByteString -> m T.Text
getMessage m = peeks $ fromMaybe ("[" <> TE.decodeLatin1 (BSS.fromShort m) <> "]") . HM.lookup m . messagesMap

listMessages :: Messages -> [(BSS.ShortByteString, T.Text)]
listMessages = HM.toList . messagesMap
