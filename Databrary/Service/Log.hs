{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Log
  ( Logs
  , MonadLog
  , initLogs
  , logMsg
  , logAccess
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Time.Format (formatTime)
import qualified Data.Traversable as Trav
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as Net
import qualified Network.Wai as Wai
import System.Locale (defaultTimeLocale)
import System.Log.FastLogger

import Databrary.Has (MonadHas, peek, peeks)
import Databrary.Model.Time

data Logs = Logs
  { loggerMessages, loggerAccess :: Maybe LoggerSet
  }

type MonadLog c m = (MonadHas Logs c m, MonadIO m)

initLog :: C.Config -> IO (Maybe LoggerSet)
initLog conf = do
  file <- C.lookup conf "file"
  buf <- C.lookupDefault defaultBufSize conf "buf"
  Trav.mapM (open buf) file
  where
  open buf "stdout" = newStdoutLoggerSet buf
  open buf "stderr" = newStderrLoggerSet buf
  open buf file = do
    num <- C.lookup conf "rotate"
    size <- C.lookupDefault (1024*1024) conf "size"
    let spec = FileLogSpec file size
    check $ spec $ fromMaybe 0 num
    Fold.mapM_ (rotate . spec) num
    newFileLoggerSet buf file

initLogs :: C.Config -> IO Logs
initLogs conf = Logs
  <$> initLog (C.subconfig "access" conf)
  <*> initLog (C.subconfig "messages" conf)

str :: ToLogStr a => a -> LogStr
str = toLogStr

char :: Char -> LogStr
char = str . BSC.singleton

pad :: ToLogStr a => Int -> a -> LogStr
pad n m
  | n < 0 = s <> p
  | otherwise = p <> s
  where
  s = str m
  p = str $ BSC.replicate (abs n - logStrLength s) ' '

quote :: Show a => a -> LogStr
quote = str . show

logTo :: (MonadLog c m, MonadHas Timestamp c m, ToLogStr a) => (Logs -> Maybe LoggerSet) -> a -> m ()
logTo w m = peeks w >>= Fold.mapM_ (\l -> do
  t :: Timestamp <- peek
  liftIO $ pushLogStr l $ str (formatTime defaultTimeLocale "%F %X " t) <> str m <> char '\n')

logMsg :: (MonadLog c m, MonadHas Timestamp c m, ToLogStr a) => a -> m ()
logMsg = logTo loggerMessages

logAccess :: (MonadLog c m, MonadHas Timestamp c m, MonadHas Wai.Request c m) => Wai.Response -> m ()
logAccess r = do
  q <- peek
  let qh = Wai.requestHeaders q
      rh = Wai.responseHeaders r
  (Just h, Nothing) <- liftIO $ Net.getNameInfo [Net.NI_NUMERICHOST] True False $ Wai.remoteHost q
  qt <- peek
  rt <- liftIO $ getCurrentTime
  logTo loggerAccess
    $ pad (-15) h
    <> pad 3 (show $ HTTP.statusCode $ Wai.responseStatus r)
    <> pad 4 (fromMaybe "-" $ lookup "user" rh)
    <> pad 4 (show (floor $ 1000 * rt `diffUTCTime` qt :: Integer))
    <> str (Wai.requestMethod q)
    <> str (Wai.rawPathInfo q)
    <> quote (fromMaybe "" $ lookup "location" rh)
    <> quote (fromMaybe "" $ lookup "referer" qh)
    <> quote (fromMaybe "" $ lookup "user-agent" qh)
