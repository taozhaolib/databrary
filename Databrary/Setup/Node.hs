module Databrary.Setup.Node
  ( nodeProgram
  , npmProgram
  , nodeInstall
  ) where

import Distribution.Simple.Program
import Distribution.Simple.Utils (rawSystemExit)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, withPrograms)
import Distribution.Verbosity (Verbosity)
import System.FilePath ((</>))

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
nodeInstall v info = do
  npm_ ["install"]
  bin <- npm ["bin"]
  rawSystemExit v (bin </> "bower") ["install"]
  where
  npm_ = rawSystemProgramConf v npmProgram (withPrograms info)
  npm = rawSystemProgramStdoutConf v npmProgram (withPrograms info)
