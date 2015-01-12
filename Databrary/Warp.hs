module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp

import Databrary.Resource (initResource)
import Databrary.App (runApp)
import Databrary.Routes (routes)
import Databrary.Wai (routeWai)

main :: IO ()
main = do
  res <- initResource
  Warp.run 8642 $ \r s ->
    runApp (routeWai routes r s) res
