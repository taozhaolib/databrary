{-# LANGUAGE OverloadedStrings #-}
module Databrary.Controller.Authorize
  ( AuthorizeTarget(..)
  , viewAuthorize
  , postAuthorize
  , deleteAuthorize
  ) where

import Control.Applicative ((<*>), (<|>))
import Control.Monad (when)
import qualified Data.Foldable as Fold
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Monoid (mempty, (<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime(..), fromGregorian, addGregorianYearsRollOver)
import Network.HTTP.Types (noContent204, StdMethod(DELETE))

import Databrary.Ops
import Databrary.Has (peek, peeks)
import qualified Databrary.JSON as JSON
import Databrary.Service.DB
import Databrary.Service.Mail
import Databrary.Model.Id
import Databrary.Model.Party
import Databrary.Model.Permission
import Databrary.Model.Identity
import Databrary.Model.Authorize
import qualified Databrary.HTTP.Route as R
import Databrary.HTTP.Form.Deform
import Databrary.Action
import Databrary.Action.Route
import Databrary.Controller.Form
import Databrary.Controller.Party
import Databrary.Controller.Angular
import Databrary.View.Authorize

data AuthorizeTarget = AuthorizeTarget
  { authorizeApply :: Bool
  , authorizeTarget :: Id Party
  }

instance R.Routable AuthorizeTarget where
  route = AuthorizeTarget <$> (False <$ "authorize" <|> True <$ "apply") <*> (Id <$> R.route)
  toRoute (AuthorizeTarget a (Id i)) = (if a then "apply" else "authorize") : R.toRoute i

viewAuthorize :: API -> PartyTarget -> AuthorizeTarget -> AppRAction
viewAuthorize api i at@(AuthorizeTarget app oi) = action GET (api, toRoute i ++ toRoute at) $ withAuth $ do
  angular
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction =<< lookupParty oi
  let (child, parent) = if app then (p, o) else (o, p)
  c <- lookupAuthorize child parent
  let c' = Authorize (Authorization mempty child parent) Nothing `fromMaybe` c
  case api of
    JSON -> okResponse [] $ JSON.Object $ authorizeJSON c'
    HTML
      | app -> okResponse [] ("" :: T.Text) -- TODO
      | otherwise -> blankForm (htmlAuthorizeForm c')

partyDelegates :: (MonadDB m, MonadHasIdentity c m) => Party -> m [Account]
partyDelegates p =
  mapMaybe partyAccount
    . (p :)
    . map (authorizeChild . authorization)
    . filter ((PermissionADMIN <=) . accessPermission)
    <$> lookupAuthorizedChildren p False

authorizeAddr :: [Either T.Text Account]
authorizeAddr = [Left "authorize@databrary.org"]

postAuthorize :: API -> PartyTarget -> AuthorizeTarget -> AppRAction
postAuthorize api i at@(AuthorizeTarget app oi) = action POST (api, i, at) $ withAuth $ do
  p <- getParty (Just PermissionADMIN) i
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
        url <- peeks $ actionURL (viewEditParty $ TargetParty $ partyId parent) . Just
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
    JSON -> maybe (emptyResponse noContent204 []) (okResponse [] . JSON.Object . authorizeJSON) a
    HTML -> redirectRouteResponse [] $ viewAuthorize api i at

deleteAuthorize :: API -> PartyTarget -> AuthorizeTarget -> AppRAction
deleteAuthorize api i at@(AuthorizeTarget app oi) = action DELETE (api, i, at) $ withAuth $ do
  p <- getParty (Just PermissionADMIN) i
  o <- maybeAction =<< lookupParty oi
  let (child, parent) = if app then (p, o) else (o, p)
  _ <- removeAuthorize $ Authorize (Authorization mempty child parent) Nothing
  case api of
    JSON -> emptyResponse noContent204 []
    HTML -> redirectRouteResponse [] $ viewAuthorize api i at
