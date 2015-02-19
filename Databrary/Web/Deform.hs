{-# LANGUAGE OverloadedStrings, PatternGuards #-}
module Databrary.Web.Deform
  ( DeformT
  , FormErrors
  , runDeform
  , (.:>)
  , withSubDeforms
  , deformCheck
  , deformOptional
  , deformNonempty
  , Deform(..) 
  , deformError
  , deformRead
  , deformRegex
  ) where

import Control.Applicative (Applicative(..), Alternative(..), (<$>), (<$), liftA2)
import Control.Arrow (first, second, (***), left)
import Control.Monad (MonadPlus(..), liftM, mapAndUnzipM, unless)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Writer.Class (MonadWriter(..))
import qualified Data.Aeson as JSON
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.HashMap.Strict as HM
import Data.Monoid (Monoid(..), (<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import qualified Network.URI as URI
import Text.Read (readEither)
import qualified Text.Regex.Posix as Regex

import Control.Has (view, peek, peeks)
import Databrary.URL
import Databrary.Web.Form

type FormErrorMessage = T.Text
type FormError = (FormPath, FormErrorMessage)
type FormErrors = [FormError]

newtype DeformT m a = DeformT { runDeformT :: Form -> m (FormErrors, Maybe a) }

instance MonadTrans DeformT where
  lift m = DeformT $ \_ -> ((,) mempty . Just) `liftM` m

instance Functor m => Functor (DeformT m) where
  fmap f (DeformT m) = DeformT $ \d ->
    second (fmap f) `fmap` m d

instance Applicative m => Applicative (DeformT m) where
  pure a = DeformT $ \_ -> pure (mempty, Just a)
  DeformT f <*> DeformT v = DeformT $ \d ->
    liftA2 k (f d) (v d) where
    k (ef, mf) (ev, mv) = (ef <> ev, mf <*> mv)

instance Monad m => Monad (DeformT m) where
  return = lift . return
  DeformT x >>= f = DeformT $ \d -> do
    (ex, mx) <- x d
    case mx of
      Nothing -> return (ex, Nothing)
      Just vx -> first (ex <>) `liftM` runDeformT (f vx) d
  fail = deformError' . T.pack

instance Monad m => MonadPlus (DeformT m) where
  mzero = DeformT $ \_ -> return (mempty, Nothing)
  DeformT a `mplus` DeformT b = DeformT $ \d -> do
    ar@(_, ma) <- a d
    case ma of
      Nothing -> b d
      Just _ -> return ar

instance (Applicative m, Monad m) => Alternative (DeformT m) where
  empty = mzero
  (<|>) = mplus

instance Monad m => MonadReader Form (DeformT m) where
  ask = DeformT $ \d -> return (mempty, Just d)
  reader f = DeformT $ \d -> return (mempty, Just (f d))
  local f (DeformT a) = DeformT $ a . f

instance Monad m => MonadWriter FormErrors (DeformT m) where
  writer (a, e) = DeformT $ \_ -> return (e, Just a)
  listen (DeformT a) = DeformT $ \d -> do
    (e, r) <- a d
    return (e, fmap (flip (,) e) r)
  pass (DeformT a) = DeformT $ \q -> do
    (e, mrf) <- a q
    case mrf of
      Just (r, f) -> return (f e, Just r)
      Nothing -> return (e, Nothing)

runDeform :: Functor m => DeformT m a -> FormData -> m (Either FormErrors a)
runDeform (DeformT fa) = fmap fr . fa . initForm where
  fr ([], Just a) = Right a
  fr (e, _) = Left e

withSubDeform :: Monad m => FormKey -> DeformT m a -> DeformT m a
withSubDeform = local . subForm

infixr 1 .:>
(.:>) :: Monad m => T.Text -> DeformT m a -> DeformT m a
(.:>) = withSubDeform . FormField

withSubDeforms :: (Functor m, Monad m) => DeformT m a -> DeformT m [a]
withSubDeforms (DeformT a) = DeformT $
  fmap (concat *** sequence) . mapAndUnzipM a . subForms

deformErrorWith :: Monad m => Maybe a -> FormErrorMessage -> DeformT m a
deformErrorWith r e = DeformT $ \f -> return ([(view f, e)], r)

deformErrorDef :: Monad m => a -> FormErrorMessage -> DeformT m a
deformErrorDef = deformErrorWith . Just

deformError :: Monad m => FormErrorMessage -> DeformT m ()
deformError = deformErrorWith (Just ())

deformError' :: Monad m => FormErrorMessage -> DeformT m a
deformError' = deformErrorWith Nothing

deformEither :: (Functor m, Monad m) => a -> Either FormErrorMessage a -> DeformT m a
deformEither def = either (deformErrorDef def) return

deformCheck :: (Functor m, Monad m) => FormErrorMessage -> (a -> Bool) -> a -> DeformT m a
deformCheck e f x = x <$ unless (f x) (deformError e)

deformOptional :: (Functor m, Monad m) => DeformT m a -> DeformT m (Maybe a)
deformOptional f = opt =<< peek where
  opt FormDatumNone = return Nothing
  opt _ = Just <$> f

deformNonempty :: (Functor m, Monad m) => DeformT m a -> DeformT m (Maybe a)
deformNonempty f = opt =<< peek where
  opt FormDatumNone = return Nothing
  opt (FormDatumBS s) | BS.null s = return Nothing
  opt (FormDatumJSON (JSON.String s)) | T.null s = return Nothing
  opt (FormDatumJSON (JSON.Object o)) | HM.null o = return Nothing
  opt (FormDatumJSON (JSON.Array v)) | V.null v = return Nothing
  opt (FormDatumJSON JSON.Null) = return Nothing
  opt _ = Just <$> f

deformParse :: (Functor m, Monad m) => a -> (FormDatum -> Either FormErrorMessage a) -> DeformT m a
deformParse def p = deformEither def =<< peeks p

class Deform a where
  deform :: (Functor m, Monad m) => DeformT m a

instance Deform a => Deform (Maybe a) where
  deform = deformNonempty deform

instance Deform T.Text where
  deform = deformParse "" fv where
    fv (FormDatumBS b) = return $ TE.decodeUtf8 b
    fv (FormDatumJSON (JSON.String t)) = return t
    fv (FormDatumJSON (JSON.Number n)) = return $ T.pack $ show n
    fv (FormDatumJSON (JSON.Bool True)) = return "1"
    fv (FormDatumJSON (JSON.Bool False)) = return ""
    fv _ = Left "Text value required"

instance Deform String where
  deform = deformParse "" fv where
    fv (FormDatumBS b) = return $ BSU.toString b
    fv (FormDatumJSON (JSON.String t)) = return $ T.unpack t
    fv (FormDatumJSON (JSON.Number n)) = return $ show n
    fv (FormDatumJSON (JSON.Bool True)) = return "1"
    fv (FormDatumJSON (JSON.Bool False)) = return ""
    fv _ = Left "String value required"

instance Deform Bool where
  deform = deformParse False fv where
    fv (FormDatumBS "true") = return True
    fv (FormDatumBS "false") = return False
    fv (FormDatumBS "on") = return True
    fv (FormDatumBS "off") = return False
    fv (FormDatumBS "1") = return True
    fv (FormDatumBS "0") = return False
    fv (FormDatumBS "") = return False
    fv (FormDatumJSON (JSON.String "true")) = return True
    fv (FormDatumJSON (JSON.String "false")) = return False
    fv (FormDatumJSON (JSON.String "on")) = return True
    fv (FormDatumJSON (JSON.String "off")) = return False
    fv (FormDatumJSON (JSON.String "1")) = return True
    fv (FormDatumJSON (JSON.String "0")) = return False
    fv (FormDatumJSON (JSON.String "")) = return False
    fv (FormDatumJSON (JSON.Number n)) = return $ n /= 0
    fv (FormDatumJSON (JSON.Bool b)) = return b
    fv (FormDatumJSON JSON.Null) = return False
    fv _ = Left "Boolean value required"

instance Deform URI where
  deform = maybe (deformErrorWith (Just URI.nullURI) "Invalid URL") return . parseURL =<< deform

deformRead :: (Functor m, Monad m) => Read a => a -> DeformT m a
deformRead def = deformEither def . left T.pack . readEither =<< deform

deformRegex :: (Functor m, Monad m) => FormErrorMessage -> Regex.Regex -> DeformT m T.Text
deformRegex err regex = deformCheck err (Regex.matchTest regex . T.unpack) =<< deform
