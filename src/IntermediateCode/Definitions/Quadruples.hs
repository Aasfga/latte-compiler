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
-- Functions
-- 
emptyQuadruplesCode :: QuadruplesCode
emptyQuadruplesCode = QuadruplesCode Map.empty

emptyFunction :: Type -> [Argument] -> FunctionCode
emptyFunction retType args = FunctionCode Map.empty retType args

getOperationType :: QuadrupleOperation -> Type
getOperationType operation = case operation of
  (ArgumentInit _ _type) -> _type
  (IntegerAdd _ _) -> Int
  (IntegerSub _ _) -> Int
  (IntegerMul _ _) -> Int
  (IntegerDiv _ _) -> Int
  (IntegerMod _ _) -> Int
  (BoolAnd _ _) -> Bool
  (BoolOr _ _) -> Bool
  (BoolNot _) -> Bool
  (StringConcat _ _) -> String
  (IntegerCompare _ _ _) -> Int
  (BoolCompare _ _ _) -> Bool
  (StringCompare _ _ _) -> String
  (ReturnValue argument) -> getQuadrupleLocationType argument
  (ReturnVoid) -> Void
  (CallFunction _ _type _) -> _type



getQuadrupleLocationType :: QuadrupleLocation -> Type
getQuadrupleLocationType (Register (TemporaryRegister _type _)) = _type
getQuadrupleLocationType (ConstInt _) = Int
getQuadrupleLocationType (ConstBool _) = Bool
getQuadrupleLocationType (ConstString _) = String
-- 
-- Patterns
-- 
pattern QuadrupleRegister :: Type -> Index -> QuadrupleLocation
pattern QuadrupleRegister _type register = Register (TemporaryRegister _type register)
