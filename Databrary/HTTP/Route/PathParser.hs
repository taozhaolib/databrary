{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators, TupleSections #-}
module Databrary.HTTP.Route.PathParser
  ( PathParser(..)
  , (</>)
  , (>/>)
  , (</<)
  , (|/|)
  , pathMaybe
  , (=/=)
  , pathElements
  , pathParser
  , pathGenerate
  , pathParserExpand
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Control.Monad (guard)
import Data.Proxy (Proxy(..))
import Data.String (IsString(..))
import qualified Data.Text as T

import qualified Databrary.Iso as I
import Databrary.Ops
import Databrary.HTTP.Route.Path

proxy :: p a -> Proxy a
proxy _ = Proxy

data PathParser a where
  PathEmpty :: PathParser ()
  PathFixed :: !T.Text -> PathParser ()
  PathDynamic :: PathDynamic a => PathParser a
  PathTrans :: (a -> b) -> (b -> a) -> PathParser a -> PathParser b
  PathTuple :: PathParser a -> PathParser b -> PathParser (a, b)
  PathEither :: PathParser a -> PathParser b -> PathParser (Either a b)

instance I.Invariant PathParser where
  invMap f' g' (PathTrans f g p) = PathTrans (f' . f) (g . g') p
  invMap f g p = PathTrans f g p

pathElements :: PathParser a -> [[PathElement]]
pathElements PathEmpty = [[]]
pathElements (PathFixed t) = [[PathElementFixed t]]
pathElements d@PathDynamic = [[PathElementDynamic (proxy d)]]
pathElements (PathTrans _ _ p) = pathElements p
pathElements (PathTuple a b) = [ ae ++ be | ae <- pathElements a, be <- pathElements b ]
pathElements (PathEither a b) = pathElements a ++ pathElements b

instance IsString (PathParser ()) where
  fromString = PathFixed . fromString

pathParse :: PathParser a -> Path -> Maybe (a, Path)
pathParse PathEmpty l = Just ((), l)
pathParse (PathFixed t) (a:l) = (, l) <$> guard (a == t)
pathParse PathDynamic (a:l) = (, l) <$> pathDynamic a
pathParse (PathTrans f _ p) a = first f <$> pathParse p a
pathParse (PathTuple p q) a = do
  (pr, a') <- pathParse p a
  first ((,) pr) <$> pathParse q a'
pathParse (PathEither p q) a = first Left <$> pathParse p a <|> first Right <$> pathParse q a
pathParse _ _ = Nothing

pathParser :: PathParser a -> Path -> Maybe a
pathParser p l = do
  (a, []) <- pathParse p l
  return a

pathGenerate :: PathParser a -> a -> Path
pathGenerate PathEmpty () = []
pathGenerate (PathFixed t) () = [t]
pathGenerate PathDynamic a = [dynamicPath a]
pathGenerate (PathTrans _ g p) a = pathGenerate p $ g a
pathGenerate (PathTuple p q) (a, b) = pathGenerate p a ++ pathGenerate q b
pathGenerate (PathEither p _) (Left a) = pathGenerate p a
pathGenerate (PathEither _ q) (Right b) = pathGenerate q b

infixr 2 </>, >/>, </<
(</>) :: PathParser a -> PathParser b -> PathParser (a, b)
(</>) = PathTuple

(>/>) :: PathParser () -> PathParser a -> PathParser a
(>/>) a b = I.second I.<$> PathTuple a b

(</<) :: PathParser a -> PathParser () -> PathParser a
(</<) a b = I.first I.<$> PathTuple a b

infix 3 |/|, =/=
(|/|) :: PathParser a -> PathParser b -> PathParser (Either a b)
(|/|) = PathEither

pathMaybe :: PathParser a -> PathParser (Maybe a)
pathMaybe p = I.right I.<$> (PathEmpty |/| p)

(=/=) :: Eq a => a -> PathParser a -> PathParser a
(=/=) a p = I.defaultEq a I.<$> pathMaybe p

pathParserExpand :: PathParser a -> [PathParser a]
pathParserExpand (PathTrans f g p) = map (PathTrans f g) $ pathParserExpand p
pathParserExpand (PathTuple a b) = [ PathTuple a' b' | a' <- pathParserExpand a, b' <- pathParserExpand b ]
pathParserExpand (PathEither a b) = map (PathTrans Left unLeft) a' ++ map (PathTrans Right unRight) b' where
  unLeft (Left x) = x
  unLeft (Right _) = error "pathParserExpand: Right"
  unRight (Left _) = error "pathParserExpand: Left"
  unRight (Right x) = x
  a' = pathParserExpand a
  b' = pathParserExpand b
pathParserExpand p = [p]

