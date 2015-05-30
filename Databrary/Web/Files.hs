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
import Control.Monad (ap, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isNothing)
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

fileNewer :: IsFilePath f => f -> WebGeneratorM Bool
fileNewer f = ReaderT $
  maybe ((True <?) =<< liftIO (fileExist f)) (\t -> (t <) . snd <$> MaybeT (fileInfo f))

whether :: Bool -> IO () -> IO Bool
whether g = (g <$) . when g

webRegenerate :: IO () -> [FilePath] -> [WebFilePath] -> WebGenerator
webRegenerate g fs ws _ = do
  wr <- or <$> mapM generateWebFile ws
  fr <- anyM (asks isNothing : map fileNewer fs)
  -- when (isNothing ft) $ createDirectoryIfMissing True $ FP.takeDirectory (webFileAbs f)
  liftIO $ whether (fr || wr) g

staticWebGenerate :: IO () -> WebGenerator
staticWebGenerate g _ = do
  t <- ask
  liftIO $ whether (isNothing t) g

webLinkDataFile :: FilePath -> WebGenerator
webLinkDataFile s f = do
  wf <- liftIO $ getDataFileName s
  webRegenerate (do
    _ <- removeFile f
    createLink wf (webFileAbs f))
    [wf] [] f
