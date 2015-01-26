{-# LANGUAGE ConstraintKinds, TemplateHaskell #-}
module Control.Has
  ( Has(..)
  , HasM
  , peek
  , peeks
  , poke
  , makeHasFor
  ) where

import Control.Applicative (Applicative)
import Control.Lens hiding (view)
import Control.Monad (unless, liftM, liftM2)
import Control.Monad.Reader (MonadReader, reader, local)
import qualified Language.Haskell.TH as TH

class Has a c where
  view :: Lens' c a
  see :: c -> a
  see = (^. view)

instance Has a a where
  view = id
  see = id

type HasM a c m = (Functor m, Applicative m, MonadReader c m, Has a c)

peek :: HasM a c m => m a
peek = reader see

peeks :: HasM a c m => (a -> b) -> m b
peeks f = reader (f . see)

poke :: HasM a c m => (a -> a) -> m r -> m r
poke = local . over view

makeHasFor :: TH.Name -> [(TH.Name, [TH.Name])] -> TH.DecsQ
makeHasFor tn fs = concatM
  (makeLensesFor [(f, f ++ "'") | (fn, _) <- fs, let f = TH.nameBase fn] tn)
  (\(fn, ts) -> do
    let ln = TH.mkName (TH.nameBase fn ++ "'")
    TH.VarI _ (TH.ArrowT `TH.AppT` t@(TH.ConT tn') `TH.AppT` ft) _ _ <- TH.reify fn
    unless (tn' == tn) $ fail $ "makeHasFor " ++ show tn ++ "." ++ show fn ++ ": field from wrong type: " ++ show tn'
    -- ts' <- TH.reifyInstances ''Has [TH.VarT (TH.mkName "a"), t]
    concatM
      [d| instance Has $(return ft) $(return t) where
            view = $(TH.varE ln)
            see = $(TH.varE fn) |]
      (\st ->
        [d| instance Has $(TH.conT st) $(return t) where
              view = $(TH.varE ln) . view
              see = see . $(TH.varE fn) |])
      ts)
  fs
  where concatM i f l = liftM2 (++) i (liftM concat $ mapM f l)
