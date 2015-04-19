{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Files
  ( webDir
  , webFile
  , findWebFiles
  , webRegenerate
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Exception (bracket)
import Control.Monad (ap)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as Fold
import System.Posix.Directory.ByteString (openDirStream, closeDirStream)
import System.Posix.Directory.Foreign (dtDir, dtReg)
import System.Posix.Directory.Traversals (readDirEnt)
import System.Posix.FilePath ((</>))

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

webFile :: RawFilePath -> RawFilePath
webFile = (webDir </>)

findWebFiles :: BS.ByteString -> IO [RawFilePath]
findWebFiles = findFiles webDir

webRegenerate :: Timestamp -> RawFilePath -> (RawFilePath -> IO ()) -> IO Bool
webRegenerate t f g = do
  fi <- fileInfo wf
  if Fold.any ((t < ) . snd) fi
    then return False
    else True <$ g wf
  where wf = webFile f
