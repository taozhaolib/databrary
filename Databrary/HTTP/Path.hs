{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Path
  ( Path
  , renderPath
  , elementsPath
  , showPathElements'
  , renderPathParser
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import Data.Typeable (typeOf)
import Network.HTTP.Types.URI (encodePathSegments)

import Databrary.HTTP.Path.Types
import Databrary.HTTP.Path.Parser

renderPath :: Path -> BSB.Builder
renderPath [] = BSB.char7 '/'
renderPath p = encodePathSegments p

elementPath :: PathElement -> Path
elementPath (PathElementFixed t) = [t]
elementPath (PathElementDynamic a) = [dynamicPath a]
elementPath (PathElementAny p) = p

elementsPath :: PathElements -> Path
elementsPath = concatMap elementPath

elementText' :: PathElement -> T.Text
elementText' (PathElementFixed t) = t
elementText' (PathElementDynamic a) = T.pack $ show $ typeOf a
elementText' (PathElementAny _) = "*"

showPathElements' :: PathElements -> String
showPathElements' = BSLC.unpack . BSB.toLazyByteString . renderPath . map elementText'

renderPathParser :: PathParser a -> a -> BSB.Builder
renderPathParser p a = renderPath $ elementsPath $ producePath p a
