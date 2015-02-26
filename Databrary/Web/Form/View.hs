{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Databrary.Web.Form.View
  ( FormViewT
  , runFormView
  , blankFormView
  , (.:>) 
  , withSubFormsViews
  , formViewErrors
  , allFormViewErrors
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow (first, second)
import Control.Monad (ap, join, liftM)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadTransControl(..))
import Data.Monoid (mempty)
import qualified Data.Text as T

import Databrary.Web.Form
import Databrary.Web.Form.Errors

newtype FormViewT m a = FormViewT { runFormViewT :: Form -> FormErrors -> m (a, FormErrors) }

lpt :: e -> a -> (a, e)
lpt e a = (a, e)

instance MonadTrans FormViewT where
  lift m = FormViewT $ \_ e ->
    liftM (lpt e) m

instance MonadTransControl FormViewT where
  type StT FormViewT a = (a, FormErrors)
  liftWith f = FormViewT $ \d e ->
    liftM (lpt e) $ f $ \t -> runFormViewT t d e
  restoreT m = FormViewT $ \_ _ -> m

instance Functor m => Functor (FormViewT m) where
  fmap f (FormViewT v) = FormViewT $ \d ->
    fmap (first f) . v d

instance (Applicative m, Monad m) => Applicative (FormViewT m) where
  pure a = FormViewT $ \_ e -> pure (a, e)
  (<*>) = ap

instance Monad m => Monad (FormViewT m) where
  return a = FormViewT $ \_ e -> return (a, e)
  FormViewT x >>= f = FormViewT $ \d e -> do
    (rx, ex) <- x d e
    runFormViewT (f rx) d ex
  fail e = FormViewT $ \_ _ -> fail e

instance Monad m => MonadReader Form (FormViewT m) where
  ask = FormViewT $ \d -> return . (,) d
  reader f = FormViewT $ \d -> return . (,) (f d)
  local f (FormViewT a) = FormViewT $ a . f

instance Monad m => MonadState FormErrors (FormViewT m) where
  get = FormViewT $ \_ -> return . join (,)
  put e = FormViewT $ \_ _ -> return ((), e)
  state f = FormViewT $ \_ -> return . f

runFormView :: Functor m => FormViewT m a -> FormData -> FormErrors -> m a
runFormView (FormViewT f) d = fmap fst . f (initForm d)

blankFormView :: Functor m => FormViewT m a -> m a
blankFormView f = runFormView f mempty mempty

withSubFormView :: Functor m => FormKey -> FormViewT m a -> FormViewT m a
withSubFormView k (FormViewT a) = FormViewT $ \d e ->
  second (setSubFormErrors e k) <$> a (subForm k d) (subFormErrors k e)

withSubFormsViews :: (Functor m, Monad m) => [a] -> (Maybe a -> FormViewT m ()) -> FormViewT m ()
withSubFormsViews l f = msfv 0 l =<< reader subForms where
  msfv _ [] [] = return ()
  msfv i xl sl = withSubFormView (FormIndex i) (f x) >> msfv (succ i) xr sr where
    (x, xr) = uncons xl
    (_, sr) = uncons sl
  uncons (x:r) = (Just x, r)
  uncons r = (Nothing, r)

infixr 2 .:>
(.:>) :: Functor m => T.Text -> FormViewT m a -> FormViewT m a
(.:>) = withSubFormView . FormField

formViewErrors :: Monad m => FormViewT m [FormErrorMessage]
formViewErrors = state $ \e -> (formErrors e, e{ formErrors = [] }) 

allFormViewErrors :: Monad m => FormViewT m [(FormPath, FormErrorMessage)]
allFormViewErrors = state $ \e -> (allFormErrors e, mempty)
