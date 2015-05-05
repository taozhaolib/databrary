{-# LANGUAGE GADTs, TupleSections, QuasiQuotes #-}
module Databrary.HTTP.Path.Parser
  ( PathParser(..)
  , (</>)
  , (</>>)
  , (>/>)
  , (</<)
  , (|/|)
  , pathMaybe
  , (=/=)
  , pathParser
  , pathGenerate
  , pathCases
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.Monad (guard)
import Data.String (IsString(..))
import qualified Data.Text as T
import Data.Typeable (cast)

import qualified Databrary.Iso as I
import Databrary.Iso.TH
import Databrary.Ops
import Databrary.HTTP.Path.Types

parserUndef :: PathParser a -> a
parserUndef _ = undefined

data PathParser a where
  PathEmpty :: PathParser ()
  PathAny :: PathParser Path
  PathFixed :: !T.Text -> PathParser ()
  PathDynamic :: PathDynamic a => PathParser a
  PathTrans :: (a -> b) -> (b -> a) -> PathParser a -> PathParser b
  PathTuple :: PathParser a -> PathParser b -> PathParser (a, b)
  PathEither :: PathParser a -> PathParser b -> PathParser (Either a b)

instance I.Invariant PathParser where
  invMap f' g' (PathTrans f g p) = PathTrans (f' . f) (g . g') p
  invMap f g p = PathTrans f g p

instance IsString (PathParser ()) where
  fromString = PathFixed . fromString

pathParse :: PathParser a -> Path -> Maybe (a, Path)
pathParse PathEmpty l = Just ((), l)
pathParse PathAny l = Just (l, [])
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
pathGenerate PathAny l = l
pathGenerate (PathFixed t) () = [t]
pathGenerate PathDynamic a = [dynamicPath a]
pathGenerate (PathTrans _ g p) a = pathGenerate p $ g a
pathGenerate (PathTuple p q) (a, b) = pathGenerate p a ++ pathGenerate q b
pathGenerate (PathEither p _) (Left a) = pathGenerate p a
pathGenerate (PathEither _ q) (Right b) = pathGenerate q b

infixr 2 </>, </>>, >/>, </<
(</>) :: PathParser a -> PathParser b -> PathParser (a, b)
(</>) = PathTuple

(</>>) :: PathParser a -> PathParser (b, c) -> PathParser (a, b, c)
(</>>) l r = [iso|(a, (b, c)) <-> (a, b, c)|] I.<$> PathTuple l r

(>/>) :: PathParser () -> PathParser a -> PathParser a
(>/>) a b = I.snd I.<$> PathTuple a b

(</<) :: PathParser a -> PathParser () -> PathParser a
(</<) a b = I.fst I.<$> PathTuple a b

infix 3 |/|, =/=
(|/|) :: PathParser a -> PathParser b -> PathParser (Either a b)
(|/|) = PathEither

pathMaybe :: PathParser a -> PathParser (Maybe a)
pathMaybe p = I.rgt I.<$> (PathEmpty |/| p)

(=/=) :: Eq a => a -> PathParser a -> PathParser a
(=/=) a p = I.defaultEq a I.<$> pathMaybe p

pathCases :: PathParser a -> [([PathElement], PathElements -> Maybe (a, PathElements))]
pathCases PathEmpty = [([], Just . (,) ())]
pathCases PathAny = [([PathElementAny undefined], rf)] where
  rf (PathElementAny p:l) = Just (p, l)
  rf _ = Nothing
pathCases (PathFixed t) = [([PathElementFixed t], rf)] where
  rf (PathElementFixed _:l) = Just ((), l)
  rf _ = Nothing
pathCases d@PathDynamic = [([PathElementDynamic (parserUndef d)], rf)] where
  rf (PathElementDynamic v:l) = (, l) <$> cast v
  rf _ = Nothing
pathCases (PathTrans f _ p) = second (fmap (first f) .) <$> pathCases p
pathCases (PathTuple a b) = do
  (ae, arf) <- pathCases a
  (be, brf) <- pathCases b
  return (ae ++ be, \r -> do
    (av, ar) <- arf r
    (bv, br) <- brf ar
    return ((av, bv), br))
pathCases (PathEither a b) =
  (second (fmap (first Left)  .) <$> pathCases a) ++
  (second (fmap (first Right) .) <$> pathCases b)
