{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Files
  ( webDir
  , genDir
  , findWebFiles
  , webRegenerate
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Exception (bracket)
import Control.Monad (ap, liftM2)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing)
import System.Posix.Directory.ByteString (openDirStream, closeDirStream)
import System.Posix.Directory.Foreign (dtDir, dtReg)
import System.Posix.Directory.Traversals (readDirEnt)
import System.Posix.Files.ByteString (removeLink)
import System.Posix.FilePath ((</>), takeDirectory)

import Databrary.Model.Time
import Databrary.Store

findFiles :: RawFilePath -> BS.ByteString -> IO [RawFilePath]
findFiles dir ext = loop "" dir where
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
        else if t == dtReg && BS.isSuffixOf ext f
          then return $ (:) (b </> f)
        else   return id)
        (ent b d dh)

webDir :: RawFilePath
webDir = "web"

genDir :: RawFilePath
genDir = "gen"

findWebFiles :: BS.ByteString -> IO [RawFilePath]
findWebFiles = findFiles webDir

webRegenerate :: Timestamp -> RawFilePath -> Maybe Timestamp -> (RawFilePath -> IO ()) -> IO Bool
webRegenerate src _ (Just dst) _ | dst >= src = return False
webRegenerate _ f ft g = True <$
  liftM2 (>>)
    (if isNothing ft
      then createDirectoryIfMissing True . unRawFilePath . takeDirectory
      else removeLink)
    g (genDir </> f)
