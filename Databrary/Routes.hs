{-# LANGUAGE OverloadedStrings #-}
module Databrary.Routes
  ( routes
  ) where

import Control.Applicative ((<$), (<|>))
import Control.Monad (msum, mfilter, guard)

import qualified Databrary.Web.Route as R
import Databrary.Action
import Databrary.Controller.Root
import Databrary.Controller.Login
import Databrary.Controller.Party
import Databrary.Controller.Volume
import Databrary.Controller.Record
import Databrary.Controller.SlotAsset
import Databrary.Controller.Citation
import Databrary.Controller.Angular
import Databrary.Controller.Static

act :: RouteAction q -> R.RouteM (Action q)
act ra = do
  R.final
  _ <- mfilter (actionMethod ra ==) R.method
  return $ routeAction ra

routes :: R.RouteM AppAction
routes = do
  api <- True <$ R.fixed "api" <|> return False
  let
    isapi = guard api
    html = guard (not api)
  msum 
    [                                 act (viewRoot api)
    , "login" >>             (html >> act viewLogin)
                                  <|> act (postLogin api)
    , R.route >>= \p ->               act (viewParty api p)
                                  <|> act (postParty api p)
               <|> (html >> "edit" >> act (viewPartyForm p))
    , "party" >>                      act (createParty api)
                                  <|> act (searchParty api)
    , R.route >>= \v ->               act (viewVolume api v)
    , R.route >>= \c ->
        R.route >>= \a ->
               (html >> "download" >> act (downloadSlotAsset c a))
    , R.route >>= \r ->               act (viewRecord api r)

    , isapi >> msum
      [ "cite" >>                     act getCitation
      ]
    , html >> msum
      [ "public" >> R.route >>=       act . staticPublicFile
      , "constants.js" >>             act angularConstants
      ]
    ]
