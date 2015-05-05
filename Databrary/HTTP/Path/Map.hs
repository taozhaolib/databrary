{-# LANGUAGE GADTs, KindSignatures, DataKinds, TypeOperators, DeriveDataTypeable #-}
module Databrary.HTTP.Path.Map
  ( PathMap
  , PathMapConflict(..)
  , empty
  , singleton
  , lookup
  , union
  , insert
  ) where

import Prelude hiding (lookup, null)

import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Exception (Exception, throw)
import Data.Hashable (Hashable(..))
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Data.Typeable (Typeable, typeRep)

import Databrary.HTTP.Path.Types

data PathDynamicRep where
  PathDynamicRep :: PathDynamic a => !(Proxy a) -> PathDynamicRep

instance Eq PathDynamicRep where
  PathDynamicRep a == PathDynamicRep b = typeRep a == typeRep b

instance Ord PathDynamicRep where
  PathDynamicRep a `compare` PathDynamicRep b = typeRep a `compare` typeRep b

instance Hashable PathDynamicRep where
  hashWithSalt s (PathDynamicRep d) = hashWithSalt s $ typeRep d

proxy :: a -> Proxy a
proxy _ = Proxy

pathDynamicRep :: PathDynamic a => a -> PathDynamicRep
pathDynamicRep = PathDynamicRep . proxy

data PathMap a
  = PathMap
  { pathMapNull :: !(Maybe a)
  , pathMapFixed :: !(HM.HashMap T.Text (PathMap a))
  , pathMapDynamic :: !(M.Map PathDynamicRep (PathMap a))
  }
  | PathMapAny
  { _pathMapAny :: a
  }

data PathMapConflict = PathMapConflict deriving (Typeable)

instance Show PathMapConflict where
  show _ = "PathMapConflict"

instance Exception PathMapConflict

empty :: PathMap a
empty = PathMap Nothing HM.empty M.empty

null :: PathMap a -> Bool
null (PathMap Nothing f d) = HM.null f && M.null d
null _ = False

lookup :: Path -> PathMap a -> Maybe (PathElements, a)
lookup [] (PathMap n _ _) = (,) [] <$> n
lookup (e:p) m@(PathMap _ f d) 
  | Just m' <- HM.lookup e f = first (PathElementFixed e :) <$> lookup p m'
  | T.null e = lookup p m
  | otherwise = ld (M.toList d) where
  ld [] = Nothing
  ld ((PathDynamicRep r,m'):l)
    | Just a <- pathDynamicAs r e = first (PathElementDynamic a :) <$> lookup p m'
    | otherwise = ld l
lookup p (PathMapAny a) = Just ([PathElementAny p], a)

singleton :: PathElements -> a -> PathMap a
singleton [] a = PathMap (Just a) HM.empty M.empty
singleton (PathElementFixed e:l) a = PathMap Nothing (HM.singleton e (singleton l a)) M.empty
singleton (PathElementDynamic e:l) a = PathMap Nothing HM.empty (M.singleton (pathDynamicRep e) (singleton l a))
singleton (PathElementAny _:_) a = PathMapAny a

unionMaybe :: Maybe a -> Maybe a -> Maybe a
unionMaybe Nothing a = a
unionMaybe a Nothing = a
unionMaybe _ _ = throw PathMapConflict

unionAny :: a -> PathMap a -> PathMap a
unionAny a m
  | null m = PathMapAny a
  | otherwise = throw PathMapConflict

union :: PathMap a -> PathMap a -> PathMap a
union (PathMap n1 f1 d1) (PathMap n2 f2 d2) =
  PathMap (unionMaybe n1 n2) (HM.unionWith union f1 f2) (M.unionWith union d1 d2)
union (PathMapAny a) m = unionAny a m
union m (PathMapAny a) = unionAny a m

insert :: PathElements -> a -> PathMap a -> PathMap a
insert [] a p@PathMap{ pathMapNull = n } = p{ pathMapNull = unionMaybe (Just a) n }
insert (PathElementFixed e:l) a p@PathMap{ pathMapFixed = f } =
  p{ pathMapFixed = HM.insertWith (const $ insert l a) e (singleton l a) f }
insert (PathElementDynamic e:l) a p@PathMap{ pathMapDynamic = d } =
  p{ pathMapDynamic = M.insertWith (const $ insert l a) (pathDynamicRep e) (singleton l a) d }
insert (PathElementAny _:_) a m = unionAny a m
insert _ _ _ = throw PathMapConflict
