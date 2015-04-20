{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Databrary.Store.Transcode
  ( startTranscode
  , collectTranscode
  ) where

import Control.Monad (guard, unless, void)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe, isNothing)
import Data.Monoid ((<>))
import System.Exit (ExitCode(..))
import System.IO (hClose)
import Text.Read (readMaybe)

import Databrary.Ops
import Databrary.Has (peek, peeks)
import Databrary.Service.DB
import Databrary.Service
import Databrary.Service.ResourceT
import Databrary.HTTP.Request
import Databrary.Model.Audit
import Databrary.Model.Segment
import Databrary.Model.Asset
import Databrary.Model.Transcode
import Databrary.Action.Route (actionRoute)
import Databrary.Store
import Databrary.Store.Types
import Databrary.Store.Temp
import Databrary.Store.Asset
import Databrary.Store.Transcoder

import {-# SOURCE #-} Databrary.Controller.Transcode

ctlTranscode :: MonadStorage c m => Transcode -> TranscodeArgs -> m (ExitCode, String, String)
ctlTranscode tc args = do
  Just ctl <- peeks storageTranscoder
  liftIO $ runTranscoder ctl ("-i" : show (transcodeId tc) : args)

transcodeArgs :: (MonadStorage c m, MonadHasRequest c m, MonadHasService c m) => Transcode -> m TranscodeArgs
transcodeArgs t@Transcode{..} = do
  Just f <- getAssetFile transcodeOrig
  req <- peek
  auth <- transcodeAuth t
  return $
    [ "-f", unRawFilePath f
    , "-r", BSC.unpack $ requestHost req <> actionRoute (remoteTranscode (transcodeId t)) <> "?auth=" <> auth
    , "--" ]
    ++ maybe [] (\l -> ["-ss", show l]) lb
    ++ maybe [] (\u -> ["-t", show $ u - fromMaybe 0 lb]) (upperBound rng)
    ++ transcodeOptions
  where
  rng = segmentRange transcodeSegment
  lb = lowerBound rng

startTranscode :: (MonadStorage c m, MonadDB m, MonadHasRequest c m, MonadHasService c m) => Transcode -> m (Maybe TranscodePID)
startTranscode tc = do
  tc' <- updateTranscode tc lock Nothing
  unless (transcodeProcess tc' == lock) $ fail $ "startTranscode " ++ show (transcodeId tc)
  args <- transcodeArgs tc
  (r, out, err) <- ctlTranscode tc' args
  let pid = guard (r == ExitSuccess) >> readMaybe out
  _ <- updateTranscode tc' pid $ (isNothing pid ?> out) <> (null err ?!> err)
  return pid
  where lock = Just (-1)

collectTranscode :: (MonadResourceT c m, MonadStorage c m, MonadAudit c m) => Transcode -> Int -> Maybe BS.ByteString -> String -> m ()
collectTranscode tc 0 sha1 logs = do
  tc' <- updateTranscode tc (Just (-2)) (Just logs)
  (f, h) <- makeTempFile
  liftIO $ hClose h
  (r, out, err) <- ctlTranscode tc ["-c", BSC.unpack $ tempFilePath f]
  _ <- updateTranscode tc' Nothing (Just $ out ++ err)
  if r /= ExitSuccess
    then fail $ "collectTranscode " ++ show (transcodeId tc) ++ ": " ++ show r ++ "\n" ++ out ++ err
    else do
      -- TODO: probe
      changeAsset (transcodeAsset tc)
        { assetSHA1 = sha1
        } (Just $ tempFilePath f)
collectTranscode tc e _ logs =
  void $ updateTranscode tc Nothing (Just $ "exit " ++ show e ++ '\n' : logs)
