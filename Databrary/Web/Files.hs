{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Files
  ( WebFilePath
  , webFileRel
  , webFileAbs
  , webFileRelRaw
  , webFileAbsRaw
  , webDir
  , webDirRaw
  , webFileRaw
  , splitWebFileExtensions
  , (<.>)
  , fileTime
  , webFileTime
  , allWebFiles
  , findWebFiles
  , WebGenerator
  , staticWebGenerate
  , webRegenerate
  , webLinkFile
  ) where

import Control.Applicative ((<$>), (<$))
import Control.Arrow (first)
import Control.Exception (bracket)
import Control.Monad (void, ap)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (isNothing)
import Data.String (IsString(..))
import System.Directory (createDirectoryIfMissing)
import qualified System.FilePath as FP
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Posix.Directory.ByteString (openDirStream, closeDirStream)
import System.Posix.Directory.Foreign (dtDir, dtReg)
import System.Posix.Directory.Traversals (readDirEnt)
import qualified System.Posix.FilePath as RFP
import System.Posix.FilePath ((</>), takeDirectory, takeExtensions, splitExtensions)
import System.Posix.Files.ByteString (createLink)

import Paths_databrary (getDataFileName)
import Databrary.Model.Time
import Databrary.Store

data WebFilePath = WebFilePath
  { webFileRel, webFileAbs :: FilePath
  , webFileRelRaw, webFileAbsRaw :: RawFilePath
  }

instance Show WebFilePath where
  showsPrec p = showsPrec p . ("web" FP.</>) . webFileRel

webDir :: FilePath
webDir = unsafeDupablePerformIO $ getDataFileName "web"

webDirRaw :: RawFilePath
webDirRaw = rawFilePath webDir

makeWebFile :: FilePath -> RawFilePath -> WebFilePath
makeWebFile f r = WebFilePath f (webDir FP.</> f) r (webDirRaw </> r)

webFile :: FilePath -> WebFilePath
webFile f = makeWebFile f (rawFilePath f)

instance IsString WebFilePath where
  fromString = webFile

webFileRaw :: RawFilePath -> WebFilePath
webFileRaw r = makeWebFile (unRawFilePath r) r

splitWebFileExtensions :: WebFilePath -> (WebFilePath, BS.ByteString)
splitWebFileExtensions f =
  first (makeWebFile (FP.dropExtensions $ webFileRel f)) $ splitExtensions $ webFileRelRaw f

(<.>) :: WebFilePath -> String -> WebFilePath
WebFilePath f fa r ra <.> e = WebFilePath (f FP.<.> e) (fa FP.<.> e) (r RFP.<.> re) (ra RFP.<.> re) where re = BSC.pack e

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
allWebFiles = map webFileRaw <$> listFiles webDirRaw

findWebFiles :: BS.ByteString -> IO [WebFilePath]
findWebFiles ext = filter ((ext ==) . takeExtensions . webFileRelRaw) <$> allWebFiles

type WebGenerator = WebFilePath -> Maybe Timestamp -> MaybeT IO Bool

webRegenerate :: Timestamp -> WebFilePath -> Maybe Timestamp -> (WebFilePath -> IO ()) -> IO Bool
webRegenerate src _ (Just dst) _ | dst >= src = return False
webRegenerate _ f ft g = True <$ do
  (if isNothing ft
    then createDirectoryIfMissing True . unRawFilePath . takeDirectory
    else void . removeFile) $ webFileAbsRaw f
  g f

staticWebGenerate :: (WebFilePath -> IO ()) -> WebGenerator
staticWebGenerate g f = lift . maybe (webRegenerate undefined f Nothing g) (const $ return False)

webLinkFile :: FilePath -> WebGenerator
webLinkFile s f t = do
  wf <- lift $ rawFilePath <$> getDataFileName s
  (_, wt) <- MaybeT $ fileInfo wf
  lift $ webRegenerate wt f t $ createLink wf . webFileAbsRaw
