module Databrary.Store.Transcoder
  ( initTranscoder
  ) where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Databrary.Store.Types

runTranscoder :: Transcoder -> [String] -> IO (String, String)
runTranscoder (Transcoder cmd arg) args = do
  (r, out, err) <- readProcessWithExitCode cmd (arg ++ args) ""
  case r of
    ExitSuccess -> return (out, err)
    ExitFailure e -> fail $ "runTranscoder " ++ intercalate " " args ++ ": " ++ show e ++ "\n" ++ out ++ "\n" ++ err

initTranscoder :: Maybe String -> Maybe FilePath -> IO (Maybe Transcoder)
initTranscoder Nothing Nothing = return Nothing
initTranscoder host dir = Just <$> do
  _ <- runTranscoder t ["-t"]
  return t
  where
  t = Transcoder "transctl.sh" $
    [ -- "-v", version
    ]
    ++ maybe [] (\d -> ["-d", d]) dir
    ++ maybe [] (\h -> ["-h", h]) host
