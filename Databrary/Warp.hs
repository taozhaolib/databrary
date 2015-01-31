module Main (main) where

import qualified Network.Wai.Handler.Warp as Warp

import Databrary.Resource (initResource)
import Databrary.App (application)

main :: IO ()
main = do
  rc <- initResource
  Warp.run 8642 $ application rc
