{
module LLVM.TableGen.Lexer
  ( scanTokens
  , Token(..)
  ) where

import           LLVM.TableGen.Prelude hiding (check)

import qualified Data.Text as Text

import           LLVM.TableGen.Lexer.Util
}

$digit            = [0-9]
$ualpha           = [a-zA-Z_]

@identifier       =  $digit* $ualpha ($ualpha | $digit)*
@decimalInteger   = [\+\-]? $digit+
@varname          = "$" $ualpha ($ualpha | $digit)*

tablegen :-

<0> {
$white+ ;
"-"         { token TokMinus }
"+"         { token TokPlus }
"("         { token TokLParen}
")"         { token TokRParen }
"{"         { token TokLBrace }
"}"         { token TokRBrace }
"["         { token TokLBracket }
"]"         { token TokRBracket }
"<"         { token TokLAngle }
">"         { token TokRAngle }
">"         { token TokRAngle }
":"         { token TokColon }
";"         { token TokSemicolon }
"."         { token TokDot }
","         { token TokComma }
"="         { token TokEq }
"?"         { token TokQuestionmark }
"#"         { token TokHash }

"//"        { startComment }

\"          { startString }

"bit"        { token TokBit }
"bits"       { token TokBits }
"class"      { token TokClass }
"code"       { token TokCode }
"dag"        { token TokDag }
"def"        { token TokDef }
"foreach"    { token TokForeach }
"defm"       { token TokDefm }
"field"      { token TokField }
"in"         { token TokIn }
"include"    { token TokInclude }
"int"        { token TokInt }
"let"        { token TokLet }
"list"       { token TokList }
"multiclass" { token TokMulticlass }
"string"     { token TokString}

@identifier { tokIdentifier }
@decimalInteger { tokDecimalInteger }
}
<comment> {
\n    { endComment }
[^\n]           ;
}
<string> {
\\ \" { emitChar' '"' }
\"    { endString }
.     { emitChar }
}

<comment> [^\n] ;

{
lexerModeInt :: LexerMode -> Int
lexerModeInt Normal = 0
lexerModeInt InString{} = string
lexerModeInt Comment = comment

scanTokens :: Text -> [Token]
scanTokens str = go (AlexInput '\n' [] str) Normal
  where
    go inp st =
      case alexScan inp (lexerModeInt st) of
        AlexEOF -> [TokEof]
        AlexError inp' ->
          [TokError (Text.cons (prevChar inp') $ Text.take 50 (currInput inp')) st]
        AlexSkip inp' _ -> go inp' st
        AlexToken inp' len act ->
          case act (Text.take len (currInput inp)) st of
            (tokens, st') -> tokens ++ go inp' st'
}
