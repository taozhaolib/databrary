import Distribution.Compat.Environment (getEnvironment)
import Distribution.PackageDescription (dataDir)
import Distribution.Simple
import Distribution.Simple.Build.PathsModule (pkgPathEnvVar)
import Distribution.Simple.BuildPaths (exeExtension)
import Distribution.Simple.LocalBuildInfo (buildDir)
import Distribution.Simple.Setup
import Distribution.Simple.Utils (rawSystemExitWithEnv)
import System.Directory (getCurrentDirectory)
import System.FilePath ((<.>), (</>))

import Databrary.Setup.Git
import Databrary.Setup.Node

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { hookedPrograms = [nodeProgram, npmProgram] ++ hookedPrograms simpleUserHooks

  , confHook = \(d, i) f -> do
    d' <- setGitVersion d
    confHook simpleUserHooks (d', i) f

  , postConf = \args flag desc info -> do
    postConf simpleUserHooks args flag desc info
    nodeInstall (fromFlag (configVerbosity flag)) info

  , buildHook = \desc info hooks flag -> do
    let verb = fromFlag (buildVerbosity flag)
    nodeModuleGenerate verb desc info
    let args = buildArgs flag
        build c = buildHook simpleUserHooks desc info hooks flag{ buildArgs = c }
    if null args
      then do
        build ["schemabrary"]
        env <- getEnvironment
        cwd <- getCurrentDirectory
        rawSystemExitWithEnv verb (buildDir info </> "schemabrary" </> "schemabrary" <.> exeExtension) []
          $ (pkgPathEnvVar desc "datadir", cwd </> dataDir desc)
          : env
        build ["databrary"]
      else
        build args

  , postBuild = \args flag desc info ->
    postBuild simpleUserHooks args flag desc info
  }
