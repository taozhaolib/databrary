{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Tag
  (
  ) where

import Databrary.Model.Tag
import Databrary.Web.Form.Deform

tagNameForm :: (Functor m, Monad m) => DeformT m TagName
tagNameForm = deformMaybe' "Invalid tag name." . validateTag =<< deform
