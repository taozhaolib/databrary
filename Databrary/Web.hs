module Databrary.Web
  ( WebFilePath
  , webFileRel
  , webFileAbs
  , webFileRelRaw
  , webFileAbsRaw
  , webDir
  , webDirRaw
  , splitWebFileExtensions
  ) where

import Control.Arrow (first)
import qualified Data.ByteString as BS
import Data.String (IsString(..))
import qualified System.FilePath as FP
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified System.Posix.FilePath as RFP

import Paths_databrary (getDataFileName)
import Databrary.Files

data WebFilePath = WebFilePath
  { webFileRel, webFileAbs :: FilePath
  , webFileRelRaw, webFileAbsRaw :: RawFilePath
  }

instance Show WebFilePath where
  showsPrec p = showsPrec p . ("web" FP.</>) . webFileRel

webDir :: FilePath
webDir = unsafeDupablePerformIO $ getDataFileName "web"

webDirRaw :: RawFilePath
webDirRaw = toRawFilePath webDir

makeWebFilePath :: FilePath -> RawFilePath -> WebFilePath
makeWebFilePath f r = WebFilePath f (webDir FP.</> f) r (webDirRaw RFP.</> r)

webFilePath :: IsFilePath f => f -> WebFilePath
webFilePath f = makeWebFilePath (toFilePath f) (toRawFilePath f)

instance IsString WebFilePath where
  fromString = webFilePath

instance IsFilePath WebFilePath where
  toFilePath = webFileAbs
  toRawFilePath = webFileAbsRaw
  fromRawFilePath = webFilePath

  WebFilePath f fa r ra </> WebFilePath f' _ r' _ = WebFilePath (f </> f') (fa </> f') (r </> r') (ra </> r')
  WebFilePath f fa r ra <.> WebFilePath f' _ r' _ = WebFilePath (f <.> f') (fa <.> f') (r <.> r') (ra <.> r')

splitWebFileExtensions :: WebFilePath -> (WebFilePath, BS.ByteString)
splitWebFileExtensions f =
  first (makeWebFilePath (FP.dropExtensions $ webFileRel f)) $ RFP.splitExtensions $ webFileRelRaw f
