import Distribution.Simple

import Databrary.Setup.Git
import Databrary.Setup.DB

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook = \(d, i) f -> do
    d' <- setGitVersion d
    confHook simpleUserHooks (d', i) f
  , postConf = \args flag desc info -> do
    postConf simpleUserHooks args flag desc info
    updateDB
  }
