{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Tag
  ( postTag
  , deleteTag
  ) where

import Control.Applicative ((<$>), (<$), (<|>), (<*>))
import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE), conflict409)

import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Slot
import Databrary.Model.Tag
import Databrary.Web.Form.Deform
import qualified Databrary.Web.Route as R
import Databrary.Action
import Databrary.Controller.Permission
import Databrary.Controller.Slot

data TagId = TagId
  { _tagIdKeyword :: Bool
  , _tagIdName :: TagName
  }

instance R.Routable TagId where
  route = TagId <$> (False <$ "tag" <|> True <$ "keyword") <*> R.route
  toRoute (TagId k n) = (if k then "keyword" else "tag") : R.toRoute n

_tagNameForm :: (Functor m, Monad m) => DeformT m TagName
_tagNameForm = deformMaybe' "Invalid tag name." . validateTag =<< deform

postTag :: API -> Id Slot -> TagId -> AppRAction
postTag api si ti@(TagId kw tn) = action POST (api, si, ti) $ withAuth $ do
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) si
  t <- addTag tn
  r <- addTagUse $ TagUse t kw u s
  guardAction r $ 
    returnResponse conflict409 [] ("The requested tag overlaps your existing tag." :: T.Text)
  okResponse [] ("" :: T.Text)

deleteTag :: API -> Id Slot -> TagId -> AppRAction
deleteTag api si ti@(TagId kw tn) = action DELETE (api, si, ti) $ withAuth $ do
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) si
  _r <- maybe (return False) (\t -> removeTagUse $ TagUse t kw u s) =<< lookupTag tn
  okResponse [] ("" :: T.Text)
