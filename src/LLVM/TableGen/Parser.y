{
module LLVM.TableGen.Parser
  ( parseDirectives
  , IncludeDirective(..)
  ) where

import LLVM.TableGen.Prelude

import LLVM.TableGen.Lexer
import LLVM.TableGen.Object
}

%tokentype { Token }
%token
'class'      { TokClass }
'def'        { TokDef }
'defm'       { TokDefm }
'include'    { TokInclude }
'int'        { TokInt }
'list'       { TokList }
'multiclass' { TokMulticlass }
'string'     { TokString }
'bit'        { TokBit }
'let'        { TokLet }
'in'         { TokIn }

'!strconcat' { TokBangStrconcat }

':'        { TokColon }
';'        { TokSemicolon }
','        { TokComma }
'('        { TokLParen }
')'        { TokRParen }
'<'        { TokLAngle }
'>'        { TokRAngle}
'{'        { TokLBrace }
'}'        { TokRBrace }
'['        { TokLBracket }
']'        { TokRBracket }
'='        { TokEq }
'.'        { TokDot }
'#'        { TokHash }

IDENTIFIER { TokIdentifier $$ }
INTEGER    { TokDecimalInt $$ }
STRING     { TokStringLit $$ }

EOF        { TokEof }

%left '#'

%monad { Either Text }
%error { errorP }

%name directives

%%
directives :: { [Either IncludeDirective Object] }
  : directivesR EOF { reverse $1 }

directivesR ::            { [Either IncludeDirective Object] }
  : {- empty -}           { [] }
  | directivesR directive { $2 : $1 }

directive :: { Either IncludeDirective Object }
  : includeDirective { Left $1 }
  | object           { Right $1 }

includeDirective :: { IncludeDirective }
  : 'include' STRING { Include $2 }

object :: { Object }
  : class      { ObjClass $1 }
  | multiclass { ObjMultiClass $1}
  | def        { ObjDef $1 }
  | defm       { ObjDefm $1 }
  | let        { ObjLet $1 }

let :: { Let }
  : 'let' letList 'in' '{' objList '}' { Let (reverse $2) (reverse $5) }
  | 'let' letList 'in' object { Let (reverse $2) [$4] }

letList :: { [LetItem] }
  : letItem { [$1] }
  | letList ',' letItem { $3 : $1 }

letItem :: { LetItem }
  : IDENTIFIER '=' value { LetItem $1 Nothing $3 }
  | IDENTIFIER rangeList '=' value { LetItem $1 (Just (reverse $2)) $4 }

rangeList :: { [RangePiece] }
  : rangePiece { [$1] }
  | rangeList ',' rangePiece { $3 : $1 }

rangePiece :: { RangePiece }
  : INTEGER { RangeInt $1 }

objList :: { [Object] }
  : {- empty -} { [] }
  | objList object { $2 : $1 }

class :: { Class }
  : 'class' IDENTIFIER templateArgList objectBody { Class $2 $3 (fst $4) (snd $4) }

multiclass :: { MultiClass }
  : 'multiclass' IDENTIFIER templateArgList baseMultiClassList '{' multiClassObjectList '}' { MultiClass $2 $3 $4 (reverse $6) }

baseMultiClassList :: { [Text] }
  : {- empty -} { [] }
  | ':' baseMultiClassListNE { reverse $2 }

baseMultiClassListNE :: { [Text] }
  : IDENTIFIER { [$1] }
  | baseMultiClassListNE ',' IDENTIFIER { $3 : $1 }

multiClassObjectList :: { [ MultiClassObject ] }
  : multiClassObject { [$1] }
  | multiClassObjectList multiClassObject { $2 : $1 }

multiClassObject :: { MultiClassObject }
  : def  { MultiDef $1 }
  | defm { MultiDefm $1 }
  | let  { MultiLet $1 }

templateArgList :: { [Declaration] }
  : {- empty -} { [] }
  | '<' templateArgListNE '>' { reverse $2 }

templateArgListNE :: { [Declaration] }
  : declaration { [$1] }
  | templateArgListNE ',' declaration { $3 : $1 }

declaration :: { Declaration }
  : type IDENTIFIER { Declaration $1 $2 Nothing }
  | type IDENTIFIER '=' value { Declaration $1 $2 (Just $4) }

type :: { Type }
  : 'int' { TyInt }
  | 'string' { TyString }
  | 'bit' { TyBit }
  | 'list' '<' type '>' { TyList $3 }
  | classId { ClassTy $1 }

classId :: { ClassId }
  : IDENTIFIER { ClassId $1 }

value :: { Value }
  : simpleValue valueSuffixes { Value $1 (reverse $2) }

valueSuffixes :: { [ValueSuffix] }
  : {- empty -} { [] }
  | valueSuffixes valueSuffix { $2 : $1 }

valueSuffix :: { ValueSuffix }
  : '.' IDENTIFIER { SuffixDot $2 }

simpleValue :: { SimpleValue }
  : IDENTIFIER { VarRef $1 }
  | INTEGER    { ValInt $1 }
  | STRING     { ValString $1 }
  | '[' valueList ']' { ValList $2 Nothing }
  | '[' valueList ']' '<' type '>' { ValList $2 (Just $5) }
  | IDENTIFIER '<' valueListNE '>' { ValAnonymousRecord $1 (reverse $3) }
  | bangOperator '(' valueListNE ')' { ValBangOp $1 Nothing $3 }
  | simpleValue '#' simpleValue { ValPaste $1 $3 }

bangOperator :: { BangOperator }
  : '!strconcat' { BangStrconcat }

def :: { Def }
  : 'def' IDENTIFIER objectBody { Def $2 (fst $3) (snd $3) }

defm :: { Defm }
  : 'defm' IDENTIFIER ':' baseClassListNE ';' { Defm $2 $4 }

objectBody :: { ([SubClassRef], Body) }
  : baseClassList body { ($1, $2) }

baseClassList :: { [SubClassRef] }
  : {- empty -} { [] }
  | ':' baseClassListNE { reverse $2 }

baseClassListNE :: { [SubClassRef] }
  : subClassRef { [$1] }
  | baseClassListNE ',' subClassRef { $3 : $1 }

subClassRef :: { SubClassRef }
  : classId { SubClassRef $1 Nothing }
  | classId '<' valueList '>' { SubClassRef $1 (Just $3) }

valueList :: { [Value] }
  : {- empty -} { [] }
  | valueListNE { reverse $1 }

valueListNE :: { [Value] }
  : value { [$1] }
  | valueListNE ',' value { $3 : $1 }

body :: { Body }
  : ';' { EmptyBody }
  | '{' bodyList '}' { BodyList $2 }

bodyList :: { [BodyItem] }
  : {- empty -} { [] }
  | bodyList bodyItem { $2 : $1 }

bodyItem :: { BodyItem }
  : declaration ';' { ItemDecl $1 }
  | 'let' letItem ';' { ItemLet $2 }

{
errorP :: [Token] -> Either Text a
errorP = Left . show . head

data IncludeDirective = Include !Text deriving (Show, Eq, Ord)

parseDirectives :: [Token] -> Either Text [Either IncludeDirective Object]
parseDirectives = directives
}
