module Main where

import LLVM.TableGen.Prelude

import Options.Applicative

import LLVM.TableGen

data Options = Options
  { optPath :: !FilePath
  , optIncludeDir :: !FilePath
  } deriving (Show, Eq, Ord)

optParser :: Parser Options
optParser =
  Options <$> strArgument (metavar "FILE") <*>
  strOption (long "include-dir" <> value "/usr/include/")

main :: IO ()
main = do
  Options path includeDir <- execParser optInfo
  print =<< parseTableGenRecursive path includeDir
  where
    optInfo = info (optParser <**> helper) fullDesc
