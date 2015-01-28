{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}
module Databrary.Action.Types
  ( ActionT
  , ActionM
  , Action
  , BAction
  , withAction
  ) where

import qualified Blaze.ByteString.Builder as Blaze
import Control.Monad.RWS.Strict (RWST, withRWST)
import Network.HTTP.Types (ResponseHeaders, Status)

type ActionT q = RWST q ResponseHeaders
type ActionM q r = ActionT q r IO
type Action q r = ActionM q r Status
type BAction q = Action q Blaze.Builder

withAction :: (q -> q') -> ActionT q' r m a -> ActionT q r m a
withAction f = withRWST $ (,) . f
