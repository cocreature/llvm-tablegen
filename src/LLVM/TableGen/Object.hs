module LLVM.TableGen.Object
  ( Object(..)
  , Class(..)
  , Def(..)
  , Defm(..)
  , Let(..)
  , LetItem(..)
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
  , ClassId(..)
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
  , classTemplateArgs :: ![Declaration]
  , classBaseClasses :: ![SubClassRef]
  , classBody :: !Body
  } deriving (Show, Eq, Ord)

data MultiClass = MultiClass
  { multiClassName :: !Text
  , multiClassTemplateArgs :: ![Declaration]
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
  , defBaseClasses :: ![SubClassRef]
  , defBody :: !Body
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

data SubClassRef = SubClassRef
  { classRefName :: !ClassId
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
  | ClassTy !ClassId
  deriving (Show, Eq, Ord)

newtype ClassId =
  ClassId Text
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
