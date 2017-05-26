module LLVM.TableGen.Lexer.Util
  ( Token(..)
  , LexerMode(..)
  , AlexInput(..)
  , token
  , tokIdentifier
  , tokDecimalInteger
  , startString
  , endString
  , emitChar
  , emitChar'
  , alexGetByte
  , startComment
  , endComment
  ) where

import           LLVM.TableGen.Prelude

import           Codec.Binary.UTF8.String (encodeChar)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

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

  | TokEof
  | TokError !Text !LexerMode
  deriving (Show, Eq, Ord)

data LexerMode
  = Normal
  | InString [Char]
  | Comment
  deriving (Show, Eq, Ord)

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

tokDecimalInteger :: Action
tokDecimalInteger lexeme mode = ([TokDecimalInt i], mode)
  where
    Right (i, _) = Text.signed Text.decimal lexeme

-- Adapted from the morte lexer
data AlexInput = AlexInput
  { prevChar :: Char
  , currBytes :: [Word8]
  , currInput :: Text
  } deriving (Show, Eq, Ord)

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput c bytes text) =
  case bytes of
    b:ytes -> Just (b, AlexInput c ytes text)
    [] ->
      case Text.uncons text of
        Nothing -> Nothing
        Just (t, ext) ->
          case encodeChar t of
            [] -> Nothing
            (b:ytes) -> Just (b, AlexInput t ytes ext)

token :: Token -> Action
token t _ mode = ([t], mode)
