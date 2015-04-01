module Databrary.Store.Transcoder
  ( runTranscoder
  , initTranscoder
  ) where

import Control.Applicative ((<$>))
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Databrary.Store.Types

runTranscoder :: Transcoder -> [String] -> IO (ExitCode, String, String)
runTranscoder (Transcoder cmd arg) args =
  readProcessWithExitCode cmd (arg ++ args) ""

initTranscoder :: Maybe String -> Maybe FilePath -> IO (Maybe Transcoder)
initTranscoder Nothing Nothing = return Nothing
initTranscoder host dir = Just <$> do
  (r, out, err) <- runTranscoder t ["-t"]
  case r of
    ExitSuccess -> return t
    ExitFailure e -> fail $ "initTranscoder test: " ++ show e ++ "\n" ++ out ++ err
  where
  t = Transcoder "./transctl.sh" $
    [ -- "-v", version
    ]
    ++ maybe [] (\d -> ["-d", d]) dir
    ++ maybe [] (\h -> ["-h", h]) host
