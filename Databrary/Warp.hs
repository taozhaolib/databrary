module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp

import Databrary.Resource (initResource)
import Databrary.App (runApp)
import Databrary.Routes (routes)
import Databrary.Action (routeAction)

main :: IO ()
main = do
  res <- initResource
  Warp.run 8642 $ \r s ->
    runApp (routeAction routes r s) res
