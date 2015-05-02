{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators #-}
module Databrary.HTTP.Route.PathMap
  ( PathMap
  , empty
  , singleton
  , lookup
  , union
  , insert
  , fromList
  ) where

import Prelude hiding (lookup)

import Control.Applicative ((<|>))
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, fromMaybe)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Data.Typeable (typeRep)

import Databrary.HTTP.Route.Path

data PathDynamicRep where
  PathDynamicRep :: PathDynamic a => !(Proxy a) -> PathDynamicRep

instance Eq PathDynamicRep where
  PathDynamicRep a == PathDynamicRep b = typeRep a == typeRep b

instance Ord PathDynamicRep where
  PathDynamicRep a `compare` PathDynamicRep b = typeRep a `compare` typeRep b

instance Hashable PathDynamicRep where
  hashWithSalt s (PathDynamicRep d) = hashWithSalt s $ typeRep d

pathCheckDynamic :: T.Text -> PathDynamicRep -> Bool
pathCheckDynamic t (PathDynamicRep p) = ok p (pathDynamic t) where
  ok :: Proxy a -> Maybe a -> Bool
  ok _ = isJust

type DynamicMap a = M.Map PathDynamicRep (PathMap a)

data PathMap a
  = PathMap
  { pathMapNull :: !(Maybe a)
  , pathMapFixed :: !(HM.HashMap T.Text (PathMap a))
  , pathMapDynamic :: !(DynamicMap a)
  }
  | PathMapAll
  { _pathMapAll :: a
  }

empty :: PathMap a
empty = PathMap Nothing HM.empty M.empty

lookupDynamic :: T.Text -> DynamicMap a -> Maybe (PathMap a)
lookupDynamic t = ld . M.toList where
  ld [] = Nothing
  ld ((r,m):l)
    | pathCheckDynamic t r = Just m
    | otherwise = ld l

lookup :: Path -> PathMap a -> Maybe a
lookup [] (PathMap n _ _) = n
lookup (e:p) m@(PathMap _ f d) 
  | T.null e = lookup p $ fromMaybe m $ HM.lookup e f
  | otherwise = lookup p =<< HM.lookup e f <|> lookupDynamic e d
lookup _ (PathMapAll a) = Just a

singleton :: [PathElement] -> a -> PathMap a
singleton [] a = PathMap (Just a) HM.empty M.empty
singleton (PathElementFixed e:l) a = PathMap Nothing (HM.singleton e (singleton l a)) M.empty
singleton (PathElementDynamic e:l) a = PathMap Nothing HM.empty (M.singleton (PathDynamicRep e) (singleton l a))

union :: PathMap a -> PathMap a -> PathMap a
union (PathMap n1 f1 d1) (PathMap n2 f2 d2) =
  PathMap (n1 <|> n2) (HM.unionWith union f1 f2) (M.unionWith union d1 d2)
union m _ = m

insert :: [PathElement] -> a -> PathMap a -> PathMap a
insert [] a p = p{ pathMapNull = Just a }
insert (PathElementFixed e:l) a p = p{ pathMapFixed = HM.insertWith union e (singleton l a) (pathMapFixed p) }
insert (PathElementDynamic e:l) a p = p{ pathMapDynamic = M.insertWith union (PathDynamicRep e) (singleton l a) (pathMapDynamic p) }

fromList :: [([PathElement], a)] -> PathMap a
fromList = foldl' (\m (l, a) -> insert l a m) empty
