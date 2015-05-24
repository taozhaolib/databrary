{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Format
  ( viewFormats
  ) where

import Control.Monad.Trans.Reader (asks)

import Databrary.HTTP.Path.Parser
import Databrary.Action
import Databrary.Action.Auth
import Databrary.View.Format

viewFormats :: AppRoute ()
viewFormats = action GET ("asset" >/> "formats") $ \() -> withoutAuth $
  okResponse [] =<< asks htmlFormats
