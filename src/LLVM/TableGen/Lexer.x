{
module LLVM.TableGen.Lexer (scanTokens) where

import           LLVM.TableGen.Prelude

import           Codec.Binary.UTF8.String
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
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


data Token
  = TokMinus
  | TokPlus
  | TokLParen
  | TokRParen
  | TokLBrace
  | TokRBrace
  | TokLBracket
  | TokRBracket
  | TokLAngle
  | TokRAngle
  | TokColon
  | TokSemicolon
  | TokDot
  | TokComma
  | TokEq
  | TokQuestionmark
  | TokHash

  | TokDecimalInt !Int

  | TokIdentifier !Text

  | TokStringLit !Text

  | TokBit
  | TokBits
  | TokClass
  | TokCode
  | TokDag
  | TokDef
  | TokForeach
  | TokDefm
  | TokField
  | TokIn
  | TokInclude
  | TokInt
  | TokLet
  | TokList
  | TokMulticlass
  | TokString

  | Eof
  | Error !Text !LexerMode
  deriving (Show, Eq, Ord)

data LexerMode
  = Normal
  | InString [Char]
  | Comment
  deriving (Show, Eq, Ord)

lexerModeInt :: LexerMode -> Int
lexerModeInt Normal = 0
lexerModeInt InString{} = string
lexerModeInt Comment = comment

type Action = Text -> LexerMode -> ([Token], LexerMode)

startComment :: Action
startComment _ _ = ([], Comment)

endComment :: Action
endComment _ _ = ([], Normal)

startString :: Action
startString _ _ = ([], InString [])

endString :: Action
endString _ (InString s) = ([TokStringLit (toS (reverse s))], Normal)

emitChar' :: Char -> Action
emitChar' c _ (InString acc) = ([], InString (c : acc))

emitChar :: Action
emitChar lexeme (InString acc) = ([], InString (reverse (toS lexeme) ++ acc))

tokIdentifier :: Action
tokIdentifier lexeme mode = ([TokIdentifier lexeme], mode)

-- TODO read does not support + as a prefix
tokDecimalInteger :: Action
tokDecimalInteger lexeme mode = ([TokDecimalInt i], mode)
  where Right (i, _) = Text.signed Text.decimal lexeme

-- Adapted from the morte lexer
data AlexInput =
  AlexInput
    { prevChar  :: Char
    , currBytes :: [Word8]
    , currInput :: Text
    }
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput c bytes text) = case bytes of
  b:ytes -> Just (b, AlexInput c ytes text)
  []     ->
    case Text.uncons text of
      Nothing       -> Nothing
      Just (t, ext) ->
        case encodeChar t of
          [] -> Nothing
          (b:ytes) -> Just (b, AlexInput t ytes ext)

token :: Token -> Action
token t _ mode = ([t], mode)

scanTokens :: Text -> [Token]
scanTokens str = go (AlexInput '\n' [] str) Normal
  where
    go inp st =
      case alexScan inp (lexerModeInt st) of
        AlexEOF -> [Eof]
        AlexError inp' -> [Error (Text.cons (prevChar inp') $ Text.take 50 (currInput inp')) st]
        AlexSkip inp' _ -> go inp' st
        AlexToken inp' len act ->
          case act (Text.take len (currInput inp)) st of
            (tokens, st') -> tokens ++ go inp' st'
}
