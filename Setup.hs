import Distribution.Simple

import Databrary.Build.Git

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook = \(d, i) f -> do
    d' <- setGitVersion d
    confHook simpleUserHooks (d', i) f
  }
