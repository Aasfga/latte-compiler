{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module IntermediateCode.Definitions.Quadruples where

import Lens.Micro.Platform
import qualified Data.Map as Map
import Types

-- 
-- Definitions
-- 
type BlockNumber = Int

data TemporaryRegister 
  = TemporaryRegister Type Index
  deriving (Eq, Ord, Show)

data QuadrupleLocation
  = Register TemporaryRegister
  | ConstInt Int 
  | ConstBool Bool
  | ConstString String
  deriving (Eq, Ord, Show)

data Quadruple 
  = QuadrupleOperation TemporaryRegister QuadrupleOperation
  | Label Label

data QuadrupleOperation
  = ArgumentInit Int Type
  | IntegerAdd QuadrupleLocation QuadrupleLocation
  | IntegerSub QuadrupleLocation QuadrupleLocation
  | IntegerMul QuadrupleLocation QuadrupleLocation
  | IntegerDiv QuadrupleLocation QuadrupleLocation
  | IntegerMod QuadrupleLocation QuadrupleLocation
  | BoolAnd QuadrupleLocation QuadrupleLocation
  | BoolOr QuadrupleLocation QuadrupleLocation
  | BoolNot QuadrupleLocation
  | StringConcat QuadrupleLocation QuadrupleLocation
  | IntegerCompare QuadrupleLocation CompareOperation QuadrupleLocation
  | BoolCompare QuadrupleLocation CompareOperation QuadrupleLocation
  | StringCompare QuadrupleLocation CompareOperation QuadrupleLocation
  | ReturnValue QuadrupleLocation
  | ReturnVoid
  | CallFunction Ident Type [QuadrupleLocation]

data QuadruplesCode 
  = QuadruplesCode {
    _functions :: Map.Map Label FunctionCode
  }

data FunctionCode
  = FunctionCode {
    _blocks :: Map.Map BlockNumber [Quadruple],
    _returnType :: Type,
    _arguments :: [Argument]
  }

$(makeLenses ''QuadruplesCode)
$(makeLenses ''FunctionCode)
-- 
-- Instances
-- 
instance HasType QuadrupleLocation where
  getType (QuadrupleRegister _type _) = _type
  getType (ConstInt _) = Int
  getType (ConstBool _) = Bool
  getType (ConstString _) = String

instance HasType QuadrupleOperation where
  getType (ArgumentInit _ _type) = _type
  getType (IntegerAdd _ _) = Int
  getType (IntegerSub _ _) = Int
  getType (IntegerMul _ _) = Int
  getType (IntegerDiv _ _) = Int
  getType (IntegerMod _ _) = Int
  getType (BoolAnd _ _) = Bool
  getType (BoolOr _ _) = Bool
  getType (BoolNot _) = Bool
  getType (StringConcat _ _) = String
  getType (IntegerCompare _ _ _) = Int
  getType (BoolCompare _ _ _) = Bool
  getType (StringCompare _ _ _) = String
  getType (ReturnValue argument) = getType argument
  getType (ReturnVoid) = Void
  getType (CallFunction _ _type _) = _type
-- 
-- Functions
-- 
emptyQuadruplesCode :: QuadruplesCode
emptyQuadruplesCode = QuadruplesCode Map.empty

emptyFunction :: Type -> [Argument] -> FunctionCode
emptyFunction retType args = FunctionCode Map.empty retType args
-- 
-- Patterns
-- 
pattern QuadrupleRegister :: Type -> Index -> QuadrupleLocation
pattern QuadrupleRegister _type register = Register (TemporaryRegister _type register)
