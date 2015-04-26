{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Files
  ( webDataFile
  , webDataFiles
  , webDir
  , findWebFiles
  , webRegenerate
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Exception (bracket)
import Control.Monad (void, ap, liftM2)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath.Posix as FP
import System.Posix.Directory.ByteString (openDirStream, closeDirStream)
import System.Posix.Directory.Foreign (dtDir, dtReg)
import System.Posix.Directory.Traversals (readDirEnt)
import System.Posix.FilePath ((</>), takeDirectory)

import Paths_databrary (getDataFileName)
import Databrary.Model.Time
import Databrary.Store

listFiles :: RawFilePath -> IO [RawFilePath]
listFiles dir = loop "" dir where
  loop b d = bracket
    (openDirStream d)
    closeDirStream
    (ent b d)
  ent b d dh = do
    (t, f) <- readDirEnt dh
    if BS.null f
      then return []
      else ap 
        (if     BSC.head f == '.'
          then return id
        else if t == dtDir
          then (++) <$> loop (b </> f) (d </> f)
        else if t == dtReg
          then return $ (:) (b </> f)
        else   return id)
        (ent b d dh)

webDataFile :: RawFilePath -> IO RawFilePath
webDataFile = fmap rawFilePath . getDataFileName . ("web" FP.</>) . unRawFilePath

webDataFiles :: IO [RawFilePath]
webDataFiles = listFiles =<< webDataFile "."

webDir :: RawFilePath
webDir = "gen"

webFiles :: IO [RawFilePath]
webFiles = listFiles webDir

findWebFiles :: BS.ByteString -> IO [RawFilePath]
findWebFiles ext = filter (BS.isSuffixOf ext) <$> webFiles

webRegenerate :: Timestamp -> RawFilePath -> Maybe Timestamp -> (RawFilePath -> IO ()) -> IO Bool
webRegenerate src _ (Just dst) _ | dst >= src = return False
webRegenerate _ f ft g = True <$
  liftM2 (>>)
    (if isNothing ft
      then createDirectoryIfMissing True . unRawFilePath . takeDirectory
      else void . removeFile)
    g (webDir </> f)
