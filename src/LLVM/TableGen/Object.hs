module LLVM.TableGen.Object
  ( Object(..)
  , Class(..)
  , Def(..)
  , Defm(..)
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
  , BangOperator(..)
  , RangePiece(..)
  , MultiClass(..)
  , MultiClassObject(..)
  ) where

import LLVM.TableGen.Prelude

data Object
  = ObjClass !Class
  | ObjMultiClass !MultiClass
  | ObjDef !Def
  | ObjDefm !Defm
  | ObjLet !Let
  deriving (Show, Eq, Ord)

data RangePiece =
  RangeInt !Int
  deriving (Show, Eq, Ord)

data Class = Class
  { className :: !Text
  , classTemplateArgList :: !(Maybe [Declaration])
  , classObjectBody :: !ObjectBody
  } deriving (Show, Eq, Ord)

data MultiClass = MultiClass
  { multiClassName :: !Text
  , multiClassTemplateArgs :: !(Maybe [Declaration])
  , multiClassBaseClasses :: ![Text]
  , mulitiClassObjects :: ![MultiClassObject]
  } deriving (Show, Eq, Ord)

data MultiClassObject
  = MultiDef !Def
  | MultiDefm !Defm
  | MultiLet !Let
  deriving (Show, Eq, Ord)

data Def = Def
  { defName :: !Text
  , defObjectBody :: !ObjectBody
  } deriving (Show, Eq, Ord)

data Defm = Defm
  { defmName :: !Text
  , defmBaseClasses :: ![SubClassRef]
  } deriving (Show, Eq, Ord)

data Let = Let
  { letList :: ![LetItem]
  , letObjects :: ![Object]
  } deriving (Show, Eq, Ord)

data LetItem =
  LetItem !Text
          !(Maybe [RangePiece])
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

data Value
  = Value !SimpleValue
          ![ValueSuffix]
  deriving (Show, Eq, Ord)

data BangOperator =
  BangStrconcat
  deriving (Show, Eq, Ord)

data SimpleValue
  = VarRef !Text
  | ValInt !Int
  | ValList ![Value]
            !(Maybe Type)
  | ValString !Text
  | ValAnonymousRecord !Text
                       ![Value]
  | ValBangOp !BangOperator
              !(Maybe Type)
              ![Value]
  | ValPaste !SimpleValue
             !SimpleValue
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

data BodyItem
  = ItemDecl !Declaration
  | ItemLet !LetItem
  deriving (Show, Eq, Ord)
