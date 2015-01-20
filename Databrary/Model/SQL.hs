{-# LANGUAGE ScopedTypeVariables, FunctionalDependencies, TemplateHaskell #-}
module Databrary.Model.SQL 
  ( SelectOutput(..)
  , Selector
  , selectColumns
  , addSelects
  , joinOn
  , maybeJoinOn
  , selectJoin
  , makeQuery
  , simpleQueryFlags
  , preparedQueryFlags
  , selectQuery'
  , selectQuery
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second)
import Control.Monad (when)
import Data.Char (isLetter, toLower)
import Data.List (intercalate, unfoldr)
import Data.String (IsString(..))
import Database.PostgreSQL.Typed.Query (QueryFlags(..), simpleQueryFlags, makePGQuery)
import qualified Language.Haskell.TH as TH

import Databrary.DB (useTPG)

data SelectOutput
  = OutputExpr String
  | OutputMap !Bool TH.Name [SelectOutput]

instance IsString SelectOutput where
  fromString = OutputExpr

selectColumn :: String -> String -> SelectOutput
selectColumn t c = OutputExpr $ t ++ '.' : c

outputTuple :: [SelectOutput] -> SelectOutput
outputTuple l = OutputMap False (TH.tupleDataName $ length l) l

outputMaybe :: SelectOutput -> SelectOutput
outputMaybe (OutputMap False f l) = OutputMap True f l
outputMaybe s = s

outputColumns :: SelectOutput -> [String]
outputColumns (OutputExpr s) = [s]
outputColumns (OutputMap _ _ o) = concatMap outputColumns o

outputParser :: SelectOutput -> [TH.Name] -> TH.Q (TH.Exp, [TH.Name])
outputParser (OutputExpr _) (i:l) = return (TH.VarE i, l)
outputParser (OutputExpr _) [] = fail "outputParser: insufficient values"
outputParser (OutputMap mb f ol) il = do
  (al, rl) <- subArgs ol il
  fi <- TH.reify f
  (fe, ft) <- case fi of
    TH.ClassOpI _ t _ _ -> return (TH.VarE f, t)
    TH.DataConI _ t _ _ -> return (TH.ConE f, t)
    TH.VarI _ t _ _ -> return (TH.VarE f, t)
    _ -> die "wrong kind"
  let am = unfoldr argMaybe ft
  flip (,) rl <$> if mb
    then do
      (bl, ae) <- bindArgs am al
      when (null bl) $ die "function with at least one non-Maybe argument required"
      return $ TH.DoE $ bl ++ [TH.NoBindS $ TH.AppE (TH.ConE 'Just) $ foldl TH.AppE fe ae]
    else return $ foldl TH.AppE fe al
  where
  subArgs [] i = return ([], i)
  subArgs (o:l) i = do
    (a, r) <- outputParser o i
    first (a :) <$> subArgs l r
  bindArgs (True:m) (a:l) = second (a:) <$> bindArgs m l
  bindArgs (False:m) (a:l) = do
    n <- TH.newName "cm"
    (bl, al) <- bindArgs m l
    return $ (TH.BindS (TH.VarP n) a : bl, TH.VarE n : al)
  bindArgs _ a = return ([], a)
  argMaybe (TH.ArrowT `TH.AppT` a `TH.AppT` r) = Just (isMaybeT a, r)
  argMaybe _ = Nothing
  isMaybeT (TH.AppT (TH.ConT m) _) = m == ''Maybe
  isMaybeT _ = False
  die s = fail $ "outputParser " ++ show f ++ ": " ++ s

data Selector = Selector
  { selectOutput :: SelectOutput
  , selectSource :: String
  , selectJoined :: String
  }

selector :: String -> SelectOutput -> Selector
selector t o = Selector o t (',':t)

selectColumns :: TH.Name -> String -> [String] -> Selector
selectColumns f t c =
  selector t (OutputMap False f $ map (selectColumn t) c)

addSelects :: TH.Name -> Selector -> [SelectOutput] -> Selector
addSelects f s c = s
  { selectOutput = OutputMap False f (selectOutput s : c) }

joinWith :: (String -> String) -> Selector -> Selector
joinWith j sel = sel{ selectJoined = j (selectSource sel) }

maybeJoinWith :: (String -> String) -> Selector -> Selector
maybeJoinWith j sel = sel
  { selectJoined = j (selectSource sel)
  , selectOutput = outputMaybe (selectOutput sel) }

joinOn :: String -> Selector -> Selector
joinOn on = joinWith (\s -> " JOIN " ++ s ++ " ON " ++ on)

maybeJoinOn :: String -> Selector -> Selector
maybeJoinOn on = maybeJoinWith (\s -> " LEFT JOIN " ++ s ++ " ON " ++ on)

selectJoin :: TH.Name -> [Selector] -> Selector
selectJoin f l@(h:t) = Selector
  { selectOutput = OutputMap False f $ map selectOutput l
  , selectSource = selectSource h ++ joins
  , selectJoined = selectJoined h ++ joins
  } where joins = concatMap selectJoined t
selectJoin _ [] = error "selectJoin: empty list"


takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = fst . foldr go ([], False) where
  go x (rest, done)
    | not done && p x = (x:rest, False)
    | otherwise = (rest, True)

makeQuery :: QueryFlags -> (String -> String) -> SelectOutput -> TH.ExpQ
makeQuery flags sql output = do
  _ <- useTPG
  nl <- mapM (TH.newName . colVar) cols
  (parse, []) <- outputParser output nl
  TH.AppE (TH.VarE 'fmap `TH.AppE` TH.LamE [TH.TupP $ map TH.VarP nl] parse)
    <$> makePGQuery flags (sql $ intercalate "," cols)
  where
  colVar s = case takeWhileEnd isLetter s of
    [] -> "c"
    (h:l) -> toLower h : l
  cols = outputColumns output

preparedQueryFlags :: QueryFlags
preparedQueryFlags = simpleQueryFlags{ flagPrepare = Just [] }

selectQueryWithFlags :: QueryFlags -> Selector -> String -> TH.ExpQ
selectQueryWithFlags flags (Selector{ selectOutput = o, selectSource = s }) sql =
  makeQuery flags (\c -> "SELECT " ++ c ++ " FROM " ++ s ++ ' ':sql) o

selectQuery :: Selector -> String -> TH.ExpQ
selectQuery = selectQueryWithFlags preparedQueryFlags

selectQuery' :: Selector -> String -> TH.ExpQ
selectQuery' = selectQueryWithFlags preparedQueryFlags{ flagNullable = Just False }
