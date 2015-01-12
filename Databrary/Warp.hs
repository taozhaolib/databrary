module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp

import Databrary.Resource
import Databrary.App
import Databrary.Action

main :: IO ()
main = do
  res <- initResource
  Warp.run 8642 $ \r s ->
    runApp (runAction defaultAction r s) res
