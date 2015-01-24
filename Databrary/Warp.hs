module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp

import Databrary.Resource (initResource)
import Databrary.Routes (routes)
import Databrary.Action.Route (routeApp)

main :: IO ()
main = do
  rc <- initResource
  Warp.run 8642 $ routeApp rc routes
