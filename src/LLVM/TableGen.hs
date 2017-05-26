module LLVM.TableGen
  (parseTableGenFile)
  where

import LLVM.TableGen.Prelude

import LLVM.TableGen.Lexer
import LLVM.TableGen.Parser
import LLVM.TableGen.Object

parseTableGenFile :: Text -> Either Text [Either IncludeDirective Object]
parseTableGenFile = parseDirectives . scanTokens
