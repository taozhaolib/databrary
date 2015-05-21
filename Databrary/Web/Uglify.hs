{-# LANGUAGE OverloadedStrings #-}
module Databrary.Web.Uglify
  ( generateUglifyJS
  ) where

import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified System.FilePath as FP
import System.Posix.FilePath ((</>), (<.>))
import System.Process (callProcess)

import Paths_databrary.Node
import Databrary.Store
import Databrary.Web.Types
import Databrary.Web.Files

generateUglifyJS :: WebGenerator
generateUglifyJS f t = do
  jl <- lift $ findWebFiles ".js"
  guard (not $ null jl)
  lift $ do
  ji <- mapM (fmap (maybe (posixSecondsToUTCTime 0) snd) . fileInfo . (webDir </>)) jl
  webRegenerate (maximum ji) f t $ \wf -> do
    let wfm = wf <.> ".map"
    callProcess (binDir FP.</> "uglifyjs") $ ["--output", unRawFilePath wf, "--source-map", unRawFilePath wfm, "--source-map-url", FP.makeRelative (unRawFilePath webDir) (unRawFilePath wfm), "--prefix", "relative", "--lint", "--screw-ie8", "--mangle", "--compress", "--define", "DEBUG=false"]
      ++ map (unRawFilePath . (webDir </>)) jl
