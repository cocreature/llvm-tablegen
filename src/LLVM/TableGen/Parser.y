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
'class'    { TokClass }
'def'      { TokDef }
'include'  { TokInclude }
'int'      { TokInt }
'list'     { TokList }
'string'   { TokString }
'bit'      { TokBit }
'let'      { TokLet }
'in'       { TokIn }

':'        { TokColon }
';'        { TokSemicolon }
','        { TokComma }
'<'        { TokLAngle }
'>'        { TokRAngle}
'{'        { TokLBrace }
'}'        { TokRBrace }
'['        { TokLBracket }
']'        { TokRBracket }
'='        { TokEq }
'.'        { TokDot }

IDENTIFIER { TokIdentifier $$ }
INTEGER    { TokDecimalInt $$ }
STRING     { TokStringLit $$ }

EOF        { TokEof }

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
  : class { ObjClass $1 }
  | def   { ObjDef $1 }
  | let   { ObjLet $1 }

let :: { Let }
  : 'let' letList 'in' '{' objList '}' { Let (reverse $2) (reverse $5) }

letList :: { [LetItem] }
  : letItem { [$1] }
  | letList ',' letItem { $3 : $1 }

letItem :: { LetItem }
  : IDENTIFIER '=' value { LetItem $1 $3 }

objList :: { [Object] }
  : {- empty -} { [] }
  | objList object { $2 : $1 }

class :: { Class }
  : 'class' IDENTIFIER templateArgList objectBody { Class $2 $3 $4 }

templateArgList :: { Maybe [Declaration] }
  : {- empty -} { Nothing }
  | '<' templateArgListR '>' { Just (reverse $2) }

templateArgListR :: { [Declaration] }
  : declaration { [$1] }
  | templateArgListR ',' declaration { $3 : $1 }

declaration :: { Declaration }
  : type IDENTIFIER { Declaration $1 $2 Nothing }
  | type IDENTIFIER '=' value { Declaration $1 $2 (Just $4) }

type :: { Type }
  : 'int' { TyInt }
  | 'string' { TyString }
  | 'bit' { TyBit }
  | 'list' '<' type '>' { TyList $3 }
  | IDENTIFIER { ClassIdentifier $1 }

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

def :: { Def }
  : 'def' IDENTIFIER objectBody { Def $2 $3 }

objectBody :: { ObjectBody }
  : baseClassList body { ObjectBody $1 $2 }

baseClassList :: { Maybe [SubClassRef] }
  : {- empty -} { Nothing }
  | ':' baseClassListNE { Just (reverse $2) }

baseClassListNE :: { [SubClassRef] }
  : subClassRef { [$1] }
  | baseClassListNE ',' subClassRef { $3 : $1 }

subClassRef :: { SubClassRef }
  : IDENTIFIER { SubClassRef $1 Nothing }
  | IDENTIFIER '<' valueList '>' { SubClassRef $1 (Just $3) }

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

{
errorP :: [Token] -> Either Text a
errorP = Left . show . head

data IncludeDirective = Include !Text deriving (Show, Eq, Ord)

parseDirectives :: [Token] -> Either Text [Either IncludeDirective Object]
parseDirectives = directives
}
