{-# LANGUAGE OverloadedStrings #-}
module Databrary.Service.Log
  ( Logs(..)
  , MonadLog
  , initLogs
  , logMsg
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Time.Format (formatTime)
import qualified Data.Traversable as Trav
import System.Locale (defaultTimeLocale)
import System.Log.FastLogger

import Databrary.Has (MonadHas, peek, peeks)
import Databrary.Model.Time

data Logs = Logs
  { logMessages, logAccess :: Maybe LoggerSet
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

logMsg :: (MonadLog c m, MonadHas Timestamp c m, ToLogStr a) => (Logs -> Maybe LoggerSet) -> a -> m ()
logMsg w m = peeks w >>= Fold.mapM_ (\l -> do
  t :: Timestamp <- peek
  liftIO $ pushLogStr l $ toLogStr (formatTime defaultTimeLocale "%F %X " t) <> toLogStr m <> toLogStr ['\n'])
