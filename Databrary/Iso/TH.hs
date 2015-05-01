{-# LANGUAGE TemplateHaskell #-}
module Databrary.Iso.TH
  ( iso
  ) where

import Data.Char (isSpace)
import Data.List (stripPrefix)
import Language.Haskell.Meta.Parse (parseExp, parsePat)
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import Databrary.Iso.Prim

split :: Eq a => [a] -> [a] -> [[a]]
split _ [] = []
split t s@(c:r) 
  | Just s' <- stripPrefix t s = [] : split t s'
  | p:l <- split t r = (c:p):l
  | otherwise = [s]

parseE :: String -> TH.ExpQ
parseE s = either (fail . (++) ("Failed to parse expression '" ++ s ++ "': ")) return $ parseExp s

parseP :: String -> TH.PatQ
parseP s = either (fail . (++) ("Failed to parse pattern '" ++ s ++ "': ")) return $ parsePat s

isoCase :: String -> TH.Q ((TH.Pat, TH.Exp), (TH.Pat, TH.Exp))
isoCase s
  | [fs, gs] <- split "<->" s = do
    fe <- parseE fs
    fp <- parseP fs
    ge <- parseE gs
    gp <- parseP gs
    return ((fp, ge), (gp, fe))
  | otherwise = fail "each iso case must contain exactly one '<->'"

isoExp :: String -> TH.ExpQ
isoExp = fmap (ie . unzip) . mapM isoCase . filter (not . all isSpace) . split "\n" where
  ie (f, g) = TH.InfixE (Just $ ce f) (TH.ConE '(:<->:)) (Just $ ce g)
  ce [(p, e)] = TH.LamE [p] e
  ce l = TH.LamCaseE [ TH.Match p (TH.NormalB e) [] | (p, e) <- l ]

iso :: QuasiQuoter
iso = QuasiQuoter
  { quoteExp = isoExp
  , quoteType = const $ fail "iso not supported in types"
  , quotePat = const $ fail "iso not supported in patterns"
  , quoteDec = const $ fail "iso not supported in declarations"
  }
