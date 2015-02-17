module Databrary.Action.Form
  ( FormErrors
  , runForm
  ) where

import Control.Applicative (Applicative(..), Alternative(..), (<$>), liftA2)
import Control.Arrow (first, second, (***))
import Control.Monad (MonadPlus(..), liftM, mapAndUnzipM)
import Control.Monad.Reader (MonadReader(..), ReaderT(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import Data.Monoid ((<>))
import qualified Data.Text as T

import Control.Has (Has(..))
import Databrary.Web.Form

data FormRequest q = FormRequest
  { requestForm :: Form
  , formRequest :: q
  }

instance Functor FormRequest where
  fmap f (FormRequest d q) = FormRequest d (f q)

instance Has a q => Has a (FormRequest q) where
  view (FormRequest _ q) = view q
instance Has Form (FormRequest q) where
  view (FormRequest f _) = f

type FormErrorMessage = T.Text
type FormError = (FormPath, FormErrorMessage)
type FormErrors = [FormError]

newtype FormActionT q m a = FormActionT { runFormActionT :: FormRequest q -> m (Maybe a, FormErrors) }

instance Functor m => Functor (FormActionT q m) where
  fmap f (FormActionT m) = FormActionT $ \q ->
    first (fmap f) `fmap` m q

instance Applicative m => Applicative (FormActionT q m) where
  pure a = FormActionT $ \_ -> pure (Just a, [])
  FormActionT f <*> FormActionT v = FormActionT $ \q ->
    liftA2 k (f q) (v q) where
    k (mf, ef) (mv, ev) = (mf <*> mv, ef <> ev)

instance Monad m => Monad (FormActionT q m) where
  return a = FormActionT $ \_ -> return (Just a, [])
  FormActionT x >>= f = FormActionT $ \q -> do
    (mx, ex) <- x q
    case mx of
      Nothing -> return (Nothing, ex)
      Just vx -> second (ex <>) `liftM` runFormActionT (f vx) q
  fail e = FormActionT $ \FormRequest{ requestForm = d } -> return (Nothing, [(formPath d, T.pack e)])

instance Monad m => MonadPlus (FormActionT q m) where
  mzero = FormActionT $ \_ -> return (Nothing, [])
  FormActionT a `mplus` FormActionT b = FormActionT $ \q -> do
    ar@(ma, _) <- a q
    case ma of
      Nothing -> b q
      Just _ -> return ar

instance (Applicative m, Monad m) => Alternative (FormActionT q m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadReader (FormRequest q) (FormActionT q m) where
  ask = FormActionT $ \q -> return (Just q, [])
  reader f = FormActionT $ \q -> return (Just (f q), [])
  local f (FormActionT a) = FormActionT $ a . f

instance Monad m => MonadWriter FormErrors (FormActionT q m) where
  writer (a, e) = FormActionT $ \_ -> return (Just a, e)
  listen (FormActionT a) = FormActionT $ \q -> do
    (r, e) <- a q
    return (fmap (flip (,) e) r, e)
  pass (FormActionT a) = FormActionT $ \q -> do
    (mrf, e) <- a q
    case mrf of
      Just (r, f) -> return (Just r, f e)
      Nothing -> return (Nothing, e)

formError :: Monad m => FormErrorMessage -> FormActionT q m ()
formError e = FormActionT $ \FormRequest{ requestForm = d } -> return (Just (), [(formPath d, e)])

runSubForm :: Monad m => FormKey -> FormActionT q m a -> FormActionT q m a
runSubForm k = local $ \d -> d{ requestForm = subForm k $ requestForm d }

runSubForms :: (Functor m, Monad m) => FormActionT q m a -> FormActionT q m [a]
runSubForms (FormActionT a) = FormActionT $ \d ->
  (sequence *** concat) <$> mapAndUnzipM (\s -> a d{ requestForm = s }) (subForms $ requestForm d)

runForm :: Functor m => FormData -> FormActionT q m a -> ReaderT q m (Either FormErrors a)
runForm fd (FormActionT fa) =
  ReaderT $ fmap fr . fa . FormRequest (initForm fd) where
  fr (Just a, []) = Right a
  fr (_, e) = Left e
