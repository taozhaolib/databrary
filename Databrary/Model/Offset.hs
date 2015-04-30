{-# LANGUAGE GeneralizedNewtypeDeriving, DataKinds, DeriveDataTypeable #-}
module Databrary.Model.Offset
  ( Offset(..)
  , offsetMillis
  ) where

import Control.Applicative ((<$>))
import Data.Fixed (Fixed(..), Milli)
import qualified Data.Text as T
import Data.Time (DiffTime)
import Data.Typeable (Typeable)
import Database.PostgreSQL.Typed.Types (PGParameter(..), PGColumn(..))
import Numeric (showSigned, showFFloat)
import qualified Text.ParserCombinators.ReadP as RP
import qualified Text.ParserCombinators.ReadPrec as RP (lift, readPrec_to_P, minPrec)
import Text.Read (readMaybe, readPrec)

import qualified Databrary.JSON as JSON

newtype Offset = Offset { offsetTime :: DiffTime } deriving (Eq, Ord, Num, Real, Fractional, RealFrac, Typeable)

offsetMillis :: Offset -> Integer
offsetMillis (Offset t) = floor (1000 * t) :: Integer

instance PGParameter "interval" Offset where
  pgEncode t (Offset o) = pgEncode t o
  pgEncodeValue e t (Offset o) = pgEncodeValue e t o
  pgLiteral t (Offset o) = pgLiteral t o
instance PGColumn "interval" Offset where
  pgDecode t = Offset . pgDecode t
  pgDecodeValue e t = Offset . pgDecodeValue e t

instance Show Offset where
  -- showsPrec p = showsPrec p . offsetMillis
  showsPrec p (Offset t) = showSigned ss p t where
    ss a =
      (if h /= 0 then shows (h :: Integer) . (':' :) else id)
      . pads m' . shows m' . (':' :)
      . pads s' . showFFloat Nothing (fromIntegral s' + realToFrac f :: Double)
      where
      (s, f) = properFraction a
      (m, s') = divMod s 60
      (h, m') = divMod m 60
      pads x 
        | x < 10 = ('0' :)
        | otherwise = id

readP :: Read a => RP.ReadP a
readP = RP.readPrec_to_P readPrec RP.minPrec

instance Read Offset where
  readPrec = RP.lift $ rm RP.<++ rc where
    -- parse milliseconds:
    rm = do
      m <- readP
      r <- RP.look
      case r of
        (':':_) -> RP.pfail
        _ -> return $ realToFrac (MkFixed m :: Milli)
    -- parse seconds with colons:
    rc = do
      pm <- RP.option '+' $ RP.satisfy (`elem` "-+")
      c <- RP.sepBy1 readP (RP.char ':')
      realToFrac . (if pm == '-' then negate else id) <$> case c of
        [s] -> return (s :: Milli)
        [m, s] -> return (60*m + s)
        [h, m, s] -> return (60*(60*h + m) + s)
        [d, h, m, s] -> return (60*(60*(24*d + h) + m) + s)
        _ -> RP.pfail

instance JSON.ToJSON Offset where
  toJSON = JSON.Number . (1000 *) . realToFrac

instance JSON.FromJSON Offset where
  parseJSON (JSON.Number ms) = return $ realToFrac $ ms / 1000
  parseJSON (JSON.String s) = maybe (fail "Invalid offset string") return $ readMaybe $ T.unpack s
  parseJSON (JSON.Bool False) = return 0
  parseJSON _ = fail "Invalid offset"

