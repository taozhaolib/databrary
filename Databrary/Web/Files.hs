{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Files
  ( fileTime
  , webFileTime
  , allWebFiles
  , findWebFiles
  , WebGenerator
  , staticWebGenerate
  , webRegenerate
  , webLinkFile
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Exception (bracket)
import Control.Monad (void, ap)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isNothing)
import System.Directory (createDirectoryIfMissing)
import System.Posix.Directory.ByteString (openDirStream, closeDirStream)
import System.Posix.Directory.Foreign (dtDir, dtReg)
import System.Posix.Directory.Traversals (readDirEnt)
import System.Posix.FilePath ((</>), takeDirectory, takeExtensions)
import System.Posix.Files.ByteString (createLink)

import Paths_databrary (getDataFileName)
import Databrary.Model.Time
import Databrary.Store
import Databrary.Web

fileTime :: RawFilePath -> MaybeT IO Timestamp
fileTime f = snd <$> MaybeT (fileInfo f)

webFileTime :: WebFilePath -> MaybeT IO Timestamp
webFileTime = fileTime . webFileAbsRaw

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
allWebFiles = map webFilePathRaw <$> listFiles webDirRaw

findWebFiles :: BS.ByteString -> IO [WebFilePath]
findWebFiles ext = filter ((ext ==) . takeExtensions . webFileRelRaw) <$> allWebFiles

type WebGenerator = WebFilePath -> Maybe Timestamp -> MaybeT IO Bool

webRegenerate :: Timestamp -> WebFilePath -> Maybe Timestamp -> IO () -> IO Bool
webRegenerate src _ (Just dst) _ | dst >= src = return False
webRegenerate _ f ft g = True <$ do
  (if isNothing ft
    then createDirectoryIfMissing True . unRawFilePath . takeDirectory
    else void . removeFile) $ webFileAbsRaw f
  g

staticWebGenerate :: WebFilePath -> IO () -> Maybe Timestamp -> MaybeT IO Bool
staticWebGenerate f g = lift . maybe (webRegenerate undefined f Nothing g) (const $ return False)

webLinkFile :: FilePath -> WebGenerator
webLinkFile s f t = do
  wf <- lift $ rawFilePath <$> getDataFileName s
  (_, wt) <- MaybeT $ fileInfo wf
  lift $ webRegenerate wt f t $ do
    _ <- removeFile (webFileAbsRaw f)
    createLink wf (webFileAbsRaw f)
