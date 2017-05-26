module LLVM.TableGen.Object
  ( Object(..)
  , Class(..)
  , Def(..)
  , Let(..)
  , LetItem(..)
  , ObjectBody(..)
  , SubClassRef(..)
  , Declaration(..)
  , Body(..)
  , Type(..)
  , Value(..)
  , SimpleValue(..)
  , ValueSuffix(..)
  , BodyItem(..)
  ) where

import LLVM.TableGen.Prelude

data Object
  = ObjClass !Class
  | ObjDef !Def
  | ObjLet !Let
  deriving (Show, Eq, Ord)

data Class = Class
  { className :: !Text
  , classTemplateArgList :: !(Maybe [Declaration])
  , classObjectBody :: !ObjectBody
  } deriving (Show, Eq, Ord)

data Def = Def
  { defName :: !Text
  , defObjectBody :: !ObjectBody
  } deriving (Show, Eq, Ord)

data Let = Let
  { letList :: ![LetItem]
  , letObjects :: ![Object]
  } deriving (Show, Eq, Ord)

data LetItem =
  LetItem !Text
          !Value
  deriving (Show, Eq, Ord)

data ObjectBody = ObjectBody
  { objBaseClassList :: !(Maybe [SubClassRef])
  , objBody :: !Body
  } deriving (Show, Eq, Ord)

data SubClassRef = SubClassRef
  { classRefName :: !Text
  , classRefArgs :: !(Maybe [Value])
  } deriving (Show, Eq, Ord)

data Value =
  Value !SimpleValue
        ![ValueSuffix]
  deriving (Show, Eq, Ord)

data SimpleValue
  = VarRef !Text
  | ValInt !Int
  | ValList ![Value]
            !(Maybe Type)
  | ValString !Text
  | ValAnonymousRecord !Text
                       ![Value]
  deriving (Show, Eq, Ord)

data ValueSuffix =
  SuffixDot !Text
  deriving (Show, Eq, Ord)

data Type
  = TyInt
  | TyString
  | TyBit
  | TyList !Type
  | ClassIdentifier !Text
  deriving (Show, Eq, Ord)

data Declaration = Declaration
  { declTy :: !Type
  , declName :: !Text
  , declVal :: !(Maybe Value)
  } deriving (Show, Eq, Ord)

data Body
  = EmptyBody
  | BodyList ![BodyItem]
  deriving (Show, Eq, Ord)

data BodyItem =
  ItemDecl !Declaration
  deriving (Show, Eq, Ord)
