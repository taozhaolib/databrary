import Distribution.Simple
import Distribution.Simple.Setup

import Databrary.Setup.Git
import Databrary.Setup.Node
import Databrary.Setup.DB

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { confHook = \(d, i) f -> do
    d' <- setGitVersion d
    confHook simpleUserHooks (d', i) f
  , hookedPrograms = [nodeProgram, npmProgram] ++ hookedPrograms simpleUserHooks
  , postConf = \args flag desc info -> do
    postConf simpleUserHooks args flag desc info
    nodeInstall (fromFlag (configVerbosity flag)) info
    -- when (Fold.or $ lookup (FlagName "devel") $ configConfigurationsFlags flag)
    updateDB
  , postBuild = \args flag desc info ->
    postBuild simpleUserHooks args flag desc info
  }
