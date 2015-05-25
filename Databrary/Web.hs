module Databrary.Web
  ( WebFilePath
  , webFileRel
  , webFileAbs
  , webFileRelRaw
  , webFileAbsRaw
  , webDir
  , webDirRaw
  , webFilePathRaw
  , splitWebFileExtensions
  , (<.>)
  ) where

import Control.Arrow (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.String (IsString(..))
import qualified System.FilePath as FP
import System.IO.Unsafe (unsafeDupablePerformIO)
import qualified System.Posix.FilePath as RFP

import Paths_databrary (getDataFileName)
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

makeWebFilePath :: FilePath -> RawFilePath -> WebFilePath
makeWebFilePath f r = WebFilePath f (webDir FP.</> f) r (webDirRaw RFP.</> r)

webFilePath :: FilePath -> WebFilePath
webFilePath f = makeWebFilePath f (rawFilePath f)

webFilePathRaw :: RawFilePath -> WebFilePath
webFilePathRaw r = makeWebFilePath (unRawFilePath r) r

instance IsString WebFilePath where
  fromString = webFilePath

splitWebFileExtensions :: WebFilePath -> (WebFilePath, BS.ByteString)
splitWebFileExtensions f =
  first (makeWebFilePath (FP.dropExtensions $ webFileRel f)) $ RFP.splitExtensions $ webFileRelRaw f

(<.>) :: WebFilePath -> String -> WebFilePath
WebFilePath f fa r ra <.> e = WebFilePath (f FP.<.> e) (fa FP.<.> e) (r RFP.<.> re) (ra RFP.<.> re) where re = BSC.pack e

