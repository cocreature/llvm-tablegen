module LLVM.TableGen
  ( parseTableGen
  , parseTableGenRecursive
  ) where

import LLVM.TableGen.Prelude

import System.FilePath

import LLVM.TableGen.Lexer
import LLVM.TableGen.Parser
import LLVM.TableGen.Object

parseTableGen :: Text -> Either Text [Either IncludeDirective Object]
parseTableGen = parseDirectives . scanTokens

-- This resolves includes automatically
parseTableGenRecursive :: FilePath -> FilePath -> IO (Either Text [Object])
parseTableGenRecursive path includeDir = do
  directivesOrErr <- parseTableGen <$> readFile path
  case directivesOrErr of
    Left err -> pure (Left err)
    Right directives -> do
      objects <- mapM resolveInclude directives
      pure $ concat <$> (sequence objects)
  where
    resolveInclude (Left (Include path')) =
      parseTableGenRecursive (includeDir </> toS path') includeDir
    resolveInclude (Right obj) = return (Right [obj])
