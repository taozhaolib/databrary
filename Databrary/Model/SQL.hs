{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, FunctionalDependencies, TemplateHaskell #-}
module Databrary.Model.SQL 
  ( SelectOutput(..)
  , Selector
  , select
  , makeQuery
  , simpleQueryFlags
  , preparedQueryFlags
  , selectQuery
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Data.Char (isLetter, toLower)
import Data.List (intercalate)
import qualified Language.Haskell.TH as TH

import Database.PostgreSQL.Typed.Query (QueryFlags(..), makePGQuery)

data SelectOutput
  = SelectExpr String
  | SelectMap TH.Exp [SelectOutput]

selectTuple :: [SelectOutput] -> SelectOutput
selectTuple l = SelectMap (TH.VarE $ TH.tupleDataName $ length l) l

selectMaybe :: SelectOutput -> SelectOutput
selectMaybe (SelectMap e l) =
  SelectMap (TH.VarE (TH.mkName ("Control.Monad.liftM" ++ show (length l))) `TH.AppE` e) l
selectMaybe s = s

selectColumns :: SelectOutput -> [String]
selectColumns (SelectExpr s) = [s]
selectColumns (SelectMap _ o) = concatMap selectColumns o

selectParser :: SelectOutput -> [TH.Exp] -> (TH.Exp, [TH.Exp])
selectParser (SelectExpr _) (i:l) = (i, l)
selectParser (SelectExpr _) [] = error "selectParser: insufficient values"
selectParser (SelectMap m ol) il =
  foldl (\(a, i) o -> first (TH.AppE a) $ selectParser o i) (m, il) ol


data Selector = Selector
  { selectOutput :: SelectOutput
  , selectSource :: String
  , selectJoined :: String
  }

select :: TH.Name -> String -> [String] -> Selector
select f t e =
  Selector (SelectMap (TH.ConE f) $ map (SelectExpr . (t ++) . ('.':)) e) t (',':t)

joinWith :: Selector -> (String -> String) -> Selector
joinWith sel j = sel{ selectJoined = j (selectSource sel) }

joinOn :: Selector -> String -> Selector
joinOn sel on = joinWith sel (\s -> " JOIN " ++ s ++ " ON " ++ on)

takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = fst . foldr go ([], False) where
  go x (rest, done)
    | not done && p x = (x:rest, False)
    | otherwise = (rest, True)

makeQuery :: QueryFlags -> (String -> String) -> SelectOutput -> TH.ExpQ
makeQuery flags sql output = do
  nl <- mapM (TH.newName . colVar) cols
  let (parse, []) = selectParser output $ map TH.VarE nl
  TH.AppE (TH.VarE 'fmap `TH.AppE` TH.LamE [TH.TupP $ map TH.VarP nl] parse)
    <$> makePGQuery flags (sql $ intercalate "," cols)
  where
  colVar s = case takeWhileEnd isLetter s of
    [] -> "c"
    (h:l) -> toLower h : l
  cols = selectColumns output

simpleQueryFlags, preparedQueryFlags :: QueryFlags
simpleQueryFlags = QueryFlags False Nothing
preparedQueryFlags = QueryFlags False (Just [])

selectQuery :: Selector -> String -> TH.ExpQ
selectQuery (Selector{ selectOutput = o, selectSource = s }) sql =
  makeQuery preparedQueryFlags (\c -> "SELECT " ++ c ++ " FROM " ++ s ++ ' ':sql) o
