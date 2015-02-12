module Databrary.URL
  ( URI
  , validHDL
  , hdlURL
  , parseURL
  ) where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.Maybe (isNothing)
import Network.URI

validHDL :: String -> Bool
validHDL = v0 (0 :: Int) where
  v0 n (c:s) | isDigit c = v1 n s
  v0 _ _ = False
  v1 n ('/':_) = n > 0
  v1 n ('.':s) = v0 (succ n) s
  v1 n s = v0 n s

hdlURL :: String -> URI
hdlURL doi = URI "hdl:" Nothing doi "" ""

parseURL :: String -> Maybe URI
parseURL d@('1':'0':'.':c:_) | isDigit c = parseURL $ "doi:" ++ d
parseURL s = do
  u <- parseURI s
  if uriScheme u `elem` ["doi:","hdl:"] && isNothing (uriAuthority u) ||
     uriScheme u == "http:" && uriAuthority u == Just (URIAuth "" "dx.doi.org" "")
    then do
      guard $ validHDL $ uriPath u
      return u
        { uriScheme = "hdl:"
        , uriAuthority = Nothing
        }
    else do
      guard $ uriScheme u `elem` ["http:","https:"]
      return u
