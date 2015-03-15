module Databrary.Model.Offset
  ( Offset
  , readsOffset
  , parseOffset
  , showOffset
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Fixed (Fixed(..), Milli)
import qualified Data.Text as T
import Data.Time (DiffTime)

import qualified Databrary.JSON as JSON

type Offset = DiffTime

readsOffset :: ReadS Offset
readsOffset os = rm os ++ rc os where
  -- parse milliseconds:
  rm i = do
    (m, r) <- reads i
    case r of
      (':':_) -> []
      _ -> return (realToFrac (MkFixed m :: Milli), r)
  -- parse colons:
  pc i = do
    (n, r) <- reads i
    first (n:) <$> case r of
      (':':r') -> pc r'
      _ -> return ([], r)
  -- parse seconds with colons:
  rc si = do
    let (sign,i) = case si of
          '-':s -> (negate, s)
          '+':s -> (id, s)
          s -> (id, s)
    (c, r) <- pc i
    flip (,) r . realToFrac . sign <$> case c of
      [s] -> return (s :: Milli)
      [m, s] -> return (60*m + s)
      [h, m, s] -> return (60*(60*h + m) + s)
      [d, h, m, s] -> return (60*(60*(24*d + h) + m) + s)
      _ -> []

parseOffset :: String -> Maybe Offset
parseOffset = rm . readsOffset where
  rm ((x,""):_) = Just x
  rm (_:l) = rm l
  rm [] = Nothing

-- ms...
showOffset :: Offset -> String
showOffset = (show :: Integer -> String) . floor . (1000 *)

instance JSON.ToJSON Offset where
  toJSON = JSON.Number . (1000 *) . realToFrac

instance JSON.FromJSON Offset where
  parseJSON (JSON.Number ms) = return $ realToFrac $ ms / 1000
  parseJSON (JSON.String s) = maybe (fail "Invalid offset string") return $ parseOffset $ T.unpack s
  parseJSON (JSON.Bool False) = return 0
  parseJSON _ = fail "Invalid offset"

