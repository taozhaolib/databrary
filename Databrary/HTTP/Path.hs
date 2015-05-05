{-# LANGUAGE OverloadedStrings #-}
module Databrary.HTTP.Path
  ( Path
  , renderPath
  , renderPathElements
  , renderPathElements'
  ) where

import qualified Data.ByteString.Builder as BSB
import qualified Data.Text as T
import Data.Typeable (typeOf)
import Network.HTTP.Types.URI (encodePathSegments)

import Databrary.HTTP.Path.Types

renderPath :: Path -> BSB.Builder
renderPath [] = BSB.char7 '/'
renderPath p = encodePathSegments p

elementPath :: PathElement -> Path
elementPath (PathElementFixed t) = [t]
elementPath (PathElementDynamic a) = [dynamicPath a]
elementPath (PathElementAny p) = p

renderPathElements :: PathElements -> BSB.Builder
renderPathElements = renderPath . concatMap elementPath

elementText' :: PathElement -> T.Text
elementText' (PathElementFixed t) = t
elementText' (PathElementDynamic a) = T.pack $ show $ typeOf a
elementText' (PathElementAny _) = "*"

renderPathElements' :: PathElements -> BSB.Builder
renderPathElements' = renderPath . map elementText'

