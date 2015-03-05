{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Authorize
  ( AuthorizeTarget(..)
  , viewAuthorize
  , postAuthorize
  ) where

import Control.Applicative ((<*>), (<|>))
import Control.Monad (when)
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime(..), fromGregorian, addGregorianYearsRollOver)

import Control.Applicative.Ops
import Control.Has (peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.DB
import Databrary.Mail
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Authorize
import qualified Databrary.Web.Route as R
import Databrary.Web.Form.Deform
import Databrary.Action
import Databrary.Action.Route
import Databrary.Controller.Form
import Databrary.Controller.Party
import Databrary.View.Authorize

data AuthorizeTarget = AuthorizeTarget
  { authorizeApply :: Bool
  , authorizeTarget :: Id Party
  }

instance R.Routable AuthorizeTarget where
  route = AuthorizeTarget <$> (False <$ "authorize" <|> True <$ "apply") <*> (Id <$> R.route)
  toRoute (AuthorizeTarget a (Id i)) = (if a then "apply" else "authorize") : R.toRoute i

viewAuthorize :: PartyTarget -> AuthorizeTarget -> AppRAction
viewAuthorize i at@(AuthorizeTarget app oi) = action GET (toRoute i ++ toRoute at) $
  withParty (Just PermissionADMIN) i $ \p -> do
    o <- maybeAction =<< lookupParty oi
    let (child, parent) = if app then (p, o) else (o, p)
    c <- lookupAuthorize child parent
    let c' = Authorize (Authorization mempty child parent) Nothing `fromMaybe` c
    if app
      then okResponse [] ("" :: String) -- TODO
      else blankForm (htmlAuthorizeForm c')

partyDelegates :: (DBM m, MonadHasIdentity c m) => Party -> m [Account]
partyDelegates p =
  mapMaybe partyAccount
    . (p :)
    . map (authorizeChild . authorization)
    . filter ((PermissionADMIN <=) . accessPermission)
    <$> lookupAuthorizedChildren p False

authorizeAddr :: [Either T.Text Account]
authorizeAddr = [Left "authorize@databrary.org"]

postAuthorize :: API -> PartyTarget -> AuthorizeTarget -> AppRAction
postAuthorize api i at@(AuthorizeTarget app oi) = action POST (api, i, at) $
  withParty (Just PermissionADMIN) i $ \p -> do
    o <- maybeAction =<< lookupParty oi
    let (child, parent) = if app then (p, o) else (o, p)
    c <- lookupAuthorize child parent
    let c' = Authorize (Authorization mempty child parent) Nothing `fromMaybe` c
    a <- if app
      then do
        when (isNothing c) $ do
          changeAuthorize c'
          dl <- partyDelegates parent
          agent <- peeks $ fmap accountEmail . partyAccount
          url <- peeks $ actionURL $ viewPartyForm $ TargetParty $ partyId parent
          sendMail
            (map Right dl ++ authorizeAddr)
            ("Databrary authorization request from " <> partyName child)
            [ partyName child, " <", fromMaybe "" agent, "> has requested to be authorized by ", partyName parent, ".\n\n\
              \To approve or reject this authorization request, go to:\n",
              TE.decodeLatin1 url, "?page=grant#auth-", T.pack (show $ partyId child), "\n\n\
              \Find more information about authorizing and managing affiliates here:\n\n\
              \http://databrary.org/access/guide/investigators/authorization/affiliates.html\n"
            ]
        return $ Just $ fromMaybe c' c
      else do
        su <- peeks identitySuperuser
        now <- peek
        let maxexp = addGregorianYearsRollOver 2 $ utctDay now
            minexp = fromGregorian 2000 1 1
        a <- runForm (api == HTML ?> htmlAuthorizeForm c') $ do
          delete <- "delete" .:> deform
          if delete then return Nothing else Just <$> do
            site <- "site" .:> deform
            member <- "member" .:> deform
            expires <- "expires" .:> (deformCheck "Expiration must be within two years." (Fold.all (\e -> su || e > minexp && e <= maxexp))
              =<< (<|> (su ?!> maxexp)) <$> deformOptional deform)
            return $ Authorize (Authorization (Access site member) child parent) $ fmap (`UTCTime` 43210) expires
        maybe (Fold.mapM_ removeAuthorize c) changeAuthorize a
        when (Fold.any ((PermissionPUBLIC <) . accessSite) a && Fold.all ((PermissionPUBLIC >=) . accessSite) c) $
          sendMail
            (maybe id (:) (Right <$> partyAccount child) authorizeAddr)
            "Databrary authorization approved"
            [ "You have been authorized for Databrary access by ", partyName parent, ".\n"
            ]
        return a
    case api of
      JSON -> okResponse [] $ maybe JSON.Null (JSON.Object . authorizeJSON) a
      HTML -> redirectRouteResponse [] $ viewAuthorize i at
