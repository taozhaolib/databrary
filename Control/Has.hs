{-# LANGUAGE ConstraintKinds, TemplateHaskell, TypeSynonymInstances, LiberalTypeSynonyms #-}
module Control.Has
  ( Has(..)
  , MonadHas
  , peek
  , peeks
  , poke
  , focus
  , makeHasFor
  , makeHasRec
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Lens hiding (view)
import Control.Monad (unless, liftM, liftM2)
import Control.Monad.Reader (MonadReader, ReaderT, reader, local, withReaderT)
import Data.List (foldl')
import qualified Language.Haskell.TH as TH

class Has a c where
  view :: Lens' c a
  see :: c -> a
  see = (^. view)

instance Has a a where
  view = id
  see = id

type MonadHas a c m = (Functor m, Applicative m, MonadReader c m, Has a c)

peek :: MonadHas a c m => m a
peek = reader see

peeks :: MonadHas a c m => (a -> b) -> m b
peeks f = reader (f . see)

poke :: MonadHas a c m => (a -> a) -> m r -> m r
poke = local . over view

focus :: Has a c => ReaderT a m r -> ReaderT c m r
focus = withReaderT peek

getFieldType :: TH.Name -> TH.Name -> TH.TypeQ
getFieldType tn fn = do
  TH.VarI _ (TH.ArrowT `TH.AppT` TH.ConT tn' `TH.AppT` ft) _ _ <- TH.reify fn
  unless (tn' == tn) $ fail $ show tn ++ "." ++ show fn ++ ": field from wrong type: " ++ show tn'
  return ft

makeHasFor :: TH.Name -> [(TH.Name, [TH.Name])] -> TH.DecsQ
makeHasFor tn fs = concatM
  ((++)
    [ TH.TySynD ht [TH.PlainTV cv] $ tupleT $
        map (\t -> TH.ConT ''Has `TH.AppT` t `TH.AppT` TH.VarT cv) (tt : concatMap (map TH.ConT . snd) fs)
    , TH.TySynD (TH.mkName ("MonadHas" ++ TH.nameBase tn)) [TH.PlainTV cv, TH.PlainTV mv] $ tupleT $
        [ TH.ConT ''Functor `TH.AppT` TH.VarT mv
        , TH.ConT ''Applicative `TH.AppT` TH.VarT mv
        , TH.ConT ''MonadReader `TH.AppT` TH.VarT cv `TH.AppT` TH.VarT mv
        , TH.ConT ht `TH.AppT` TH.VarT cv
        ]
    ]
    <$> makeLensesFor [(f, f ++ "'") | (fn, _) <- fs, let f = TH.nameBase fn] tn)
  (\(fn, ts) -> do
    let ln = TH.mkName (TH.nameBase fn ++ "'")
    ft <- getFieldType tn fn
    concatM
      [d| instance Has $(return ft) $(return tt) where
            view = $(TH.varE ln)
            see = $(TH.varE fn) |]
      (\st ->
        [d| instance Has $(TH.conT st) $(return tt) where
              view = $(TH.varE ln) . view
              see = see . $(TH.varE fn) |])
      ts)
  fs
  where
  tt = TH.ConT tn
  ht = TH.mkName ("Has" ++ TH.nameBase tn)
  cv = TH.mkName "c"
  mv = TH.mkName "m"
  concatM i f l = liftM2 (++) i (liftM concat $ mapM f l)
  tupleT l = foldl' TH.AppT (TH.TupleT (length l)) l

makeHasRec :: TH.Name -> [TH.Name] -> TH.DecsQ
makeHasRec tn fs = do
  TH.ClassI _ il <- TH.reify ''Has
  makeHasFor tn =<< mapM (\fn -> do
    ft <- getFieldType tn fn
    return (fn, [ st
      | TH.InstanceD _ (TH.ConT hs `TH.AppT` TH.ConT st `TH.AppT` ft') _ <- il
      , hs == ''Has
      , ft' == ft 
      ]))
    fs
