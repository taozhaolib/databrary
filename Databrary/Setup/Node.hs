module Databrary.Setup.Node
  ( nodeProgram
  , npmProgram
  , nodeInstall
  , nodeModuleGenerate
  ) where

import Data.List (intercalate)
import qualified Distribution.ModuleName as Mod
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.BuildPaths (autogenModulesDir, autogenModuleName)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, withPrograms)
import Distribution.Simple.Program
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile)
import Distribution.Text (display)
import Distribution.Verbosity (Verbosity)
import System.FilePath ((</>), (<.>), takeDirectory)

nodeProgram :: Program
nodeProgram = (simpleProgram "node")
  { programFindVersion = findProgramVersion "-v" ver
  } where
  ver ('v':s) = s
  ver _ = ""

npmProgram :: Program
npmProgram = (simpleProgram "npm")
  { programFindVersion = findProgramVersion "-v" id
  {-
  , programPostConf = \v p -> do
    bin <- rawSystemProgramStdout v p ["bin"]
    return p
      { programProperties = Map.insert "bin" bin $ programProperties p
      }
  -}
  }

nodeInstall :: Verbosity -> LocalBuildInfo -> IO ()
nodeInstall verb info = rawSystemProgramConf verb npmProgram (withPrograms info) ["install"]

nodeModuleGenerate :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
nodeModuleGenerate verb desc info = do
  bin <- rawSystemProgramStdoutConf verb npmProgram (withPrograms info) ["bin"]
  createDirectoryIfMissingVerbose verb True $ takeDirectory file
  rewriteFile file
    $ "module " ++ display mod ++ "(binDir) where\n\
    \binDir :: FilePath\n\
    \binDir = " ++ show bin ++ "\n"
  where
  file = autogenModulesDir info </> Mod.toFilePath mod <.> "hs"
  mod = Mod.fromString $ intercalate "." $ Mod.components (autogenModuleName desc) ++ ["Node"]
