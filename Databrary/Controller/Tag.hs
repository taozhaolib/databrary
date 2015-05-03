{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Databrary.Controller.Tag
  ( postTag
  , deleteTag
  ) where

import qualified Data.Text as T
import Network.HTTP.Types (StdMethod(DELETE), conflict409)

import qualified Databrary.Iso as I
import Databrary.Iso.TH
import Databrary.Model.Permission
import Databrary.Model.Id
import Databrary.Model.Slot
import Databrary.Model.Tag
import Databrary.HTTP.Form.Deform
import Databrary.HTTP.Route.PathParser
import Databrary.Action
import Databrary.Controller.Permission
import Databrary.Controller.Slot

data TagId = TagId
  { _tagIdKeyword :: Bool
  , _tagIdName :: TagName
  }

pathTagId :: PathParser TagId
pathTagId = [iso|(b, t) <-> TagId b t|] I.<$>
  (I.isRight I.<$> ("tag" |/| "keyword") </> PathDynamic)

_tagNameForm :: (Functor m, Monad m) => DeformT m TagName
_tagNameForm = deformMaybe' "Invalid tag name." . validateTag =<< deform

postTag :: AppRoute (API, Id Slot, TagId)
postTag = action POST (pathAPI </>> pathSlotId </> pathTagId) $ \(api, si, TagId kw tn) -> withAuth $ do
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) si
  t <- addTag tn
  r <- addTagUse $ TagUse t kw u s
  guardAction r $ 
    returnResponse conflict409 [] ("The requested tag overlaps your existing tag." :: T.Text)
  okResponse [] ("" :: T.Text)

deleteTag :: AppRoute (API, Id Slot, TagId)
deleteTag = action DELETE (pathAPI </>> pathSlotId </> pathTagId) $ \(api, si, TagId kw tn) -> withAuth $ do
  u <- authAccount
  s <- getSlot (if kw then PermissionEDIT else PermissionSHARED) si
  _r <- maybe (return False) (\t -> removeTagUse $ TagUse t kw u s) =<< lookupTag tn
  okResponse [] ("" :: T.Text)
