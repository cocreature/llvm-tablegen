module LLVM.TableGen.Object
  ( Object(..)
  , Class(..)
  , Def(..)
  , ObjectBody(..)
  , SubClassRef(..)
  , Declaration(..)
  , Body(..)
  , Type(..)
  , Value(..)
  , SimpleValue(..)
  , BodyItem(..)
  ) where

import LLVM.TableGen.Prelude

data Object
  = ObjClass !Class
  | ObjDef !Def
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

data ObjectBody = ObjectBody
  { objBaseClassList :: !(Maybe [SubClassRef])
  , objBody :: !Body
  } deriving (Show, Eq, Ord)

data SubClassRef = SubClassRef
  { classRefName :: !Text
  } deriving (Show, Eq, Ord)

data Value =
  Value !SimpleValue
  deriving (Show, Eq, Ord)

data SimpleValue =
  VarRef !Text
  deriving (Show, Eq, Ord)

data Type =
  TyInt
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
