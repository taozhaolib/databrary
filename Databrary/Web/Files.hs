{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Files
  ( fileNewer
  , allWebFiles
  , findWebFiles
  , staticWebGenerate
  , webRegenerate
  , webLinkDataFile
  ) where

import Control.Exception (bracket)
import Control.Monad (ap, when, unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Function (on)
import Data.Maybe (isNothing, fromJust)
-- import System.Directory (createDirectoryIfMissing)
import System.Posix.Directory.ByteString (openDirStream, closeDirStream)
import System.Posix.Directory.Foreign (dtDir, dtReg)
import System.Posix.Directory.Traversals (readDirEnt)
import System.Posix.FilePath (takeExtensions)
import System.Posix.Files (createLink)

import Paths_databrary (getDataFileName)
import Databrary.Ops
import Databrary.Files
import Databrary.Web
import Databrary.Web.Types
import {-# SOURCE #-} Databrary.Web.Rules

listFiles :: RawFilePath -> IO [RawFilePath]
listFiles dir = loop "" where
  loop b = bracket
    (openDirStream (dir </> b))
    closeDirStream
    (ent b)
  ent b dh = do
    (t, f) <- readDirEnt dh
    if BS.null f
      then return []
      else ap 
        (if     BSC.head f == '.'
          then return id
        else if t == dtDir
          then (++) <$> loop (b </> f)
        else if t == dtReg
          then return $ (:) (b </> f)
        else   return id)
        (ent b dh)

allWebFiles :: IO [WebFilePath]
allWebFiles = map fromRawFilePath <$> listFiles webDirRaw

findWebFiles :: BS.ByteString -> IO [WebFilePath]
findWebFiles ext = filter ((ext ==) . takeExtensions . webFileRelRaw) <$> allWebFiles

anyM :: Monad m => [m Bool] -> m Bool
anyM [] = return False
anyM (a:l) = do
  r <- a
  if r then return True else anyM l

fileNotFound :: IsFilePath f => f -> WebGeneratorM a
fileNotFound f = throwError $ toFilePath f ++ " not found"

fileNewer :: IsFilePath f => f -> WebGenerator
fileNewer f (_, Nothing) = do
  e <- liftIO $ fileExist f
  unless e $ fileNotFound f
  return True
fileNewer f (_, Just o) =
  maybe (fileNotFound f) (return . (webFileTimestamp o <) . snd)
    =<< liftIO (fileInfo f)

whether :: Bool -> IO () -> IO Bool
whether g = (g <$) . when g

webRegenerate :: IO () -> [FilePath] -> [WebFilePath] -> WebGenerator
webRegenerate g fs ws fo@(_, o) = do
  wr <- mapM generateWebFile ws
  fr <- anyM (return (isNothing o) : map (`fileNewer` fo) fs)
  -- when (isNothing ft) $ createDirectoryIfMissing True $ FP.takeDirectory (webFileAbs f)
  liftIO $ whether (fr || any (on (<) webFileTimestamp (fromJust o)) wr) g

staticWebGenerate :: IO () -> WebGenerator
staticWebGenerate g (_, o) =
  liftIO $ whether (isNothing o) g

webLinkDataFile :: FilePath -> WebGenerator
webLinkDataFile s fo@(f, _) = do
  wf <- liftIO $ getDataFileName s
  webRegenerate (do
    _ <- removeFile f
    createLink wf (webFileAbs f))
    [wf] [] fo
