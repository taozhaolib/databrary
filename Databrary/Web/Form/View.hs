{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Form.View
  ( FormView
  , runFormView
  , (.:>) 
  , formViewErrors
  , allFormViewErrors
  ) where

import Control.Applicative (Applicative(..))
import Control.Arrow (first, second)
import Control.Monad (ap, join)
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.State (MonadState(..))
import Data.Monoid (mempty)
import qualified Data.Text as T

import Databrary.Web.Form
import Databrary.Web.Form.Errors

newtype FormView a = FormView { unFormView :: Form -> FormErrors -> (a, FormErrors) }

instance Functor FormView where
  fmap f (FormView v) = FormView $ \d -> first f . v d

instance Applicative FormView where
  pure = return
  (<*>) = ap

instance Monad FormView where
  return a = FormView $ \_ -> (,) a
  x >>= f = FormView $ \d e ->
    let run v = unFormView v d
        (rx, ex) = run x e in run (f rx) ex

instance MonadReader Form FormView where
  ask = FormView (,)
  reader f = FormView $ (,) . f
  local f (FormView a) = FormView $ a . f

instance MonadState FormErrors FormView where
  get = FormView $ \_ -> join (,)
  put e = FormView $ \_ _ -> ((), e)
  state f = FormView $ \_ -> f

runFormView :: FormView a -> FormData -> FormErrors -> a
runFormView (FormView f) = (fst .) . f . initForm

withSubFormView :: FormKey -> FormView a -> FormView a
withSubFormView k (FormView a) = FormView $ \d e ->
  second (setSubFormErrors e k) $ a (subForm k d) (subFormErrors k e)

infixr 2 .:>
(.:>) :: T.Text -> FormView a -> FormView a
(.:>) = withSubFormView . FormField

formViewErrors :: FormView [FormErrorMessage]
formViewErrors = state $ \e -> (formErrors e, e{ formErrors = [] }) 

allFormViewErrors :: FormView [(FormPath, FormErrorMessage)]
allFormViewErrors = state $ \e -> (allFormErrors e, mempty)
