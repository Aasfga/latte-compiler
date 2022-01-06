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

data QuadrupleArgument
  = Register TemporaryRegister
  | ConstValue Value
  deriving (Eq, Ord, Show)

data Quadruple 
  = QuadrupleOperation TemporaryRegister QuadrupleOperation
  | Label Label

data QuadrupleOperation
  = ArgumentInit Int Type
  | IntegerAdd QuadrupleArgument QuadrupleArgument
  | IntegerSub QuadrupleArgument QuadrupleArgument
  | IntegerMul QuadrupleArgument QuadrupleArgument
  | IntegerDiv QuadrupleArgument QuadrupleArgument
  | IntegerMod QuadrupleArgument QuadrupleArgument
  | BoolAnd QuadrupleArgument QuadrupleArgument
  | BoolOr QuadrupleArgument QuadrupleArgument
  | BoolNot QuadrupleArgument
  | StringConcat QuadrupleArgument QuadrupleArgument
  | IntegerCompare QuadrupleArgument CompareOperation QuadrupleArgument
  | BoolCompare QuadrupleArgument CompareOperation QuadrupleArgument
  | StringCompare QuadrupleArgument CompareOperation QuadrupleArgument
  | ReturnValue QuadrupleArgument
  | ReturnVoid
  | CallFunction Ident Type [QuadrupleArgument]

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
  (ReturnValue argument) -> getQuadrupleArgumentType argument
  (ReturnVoid) -> Void
  (CallFunction _ _type _) -> _type



getQuadrupleArgumentType :: QuadrupleArgument -> Type
getQuadrupleArgumentType (Register (TemporaryRegister _type _)) = _type
getQuadrupleArgumentType (ConstValue (IntValue _)) = Int
getQuadrupleArgumentType (ConstValue (BoolValue _)) = Bool
getQuadrupleArgumentType (ConstValue (StringValue _)) = String
-- 
-- Patterns
-- 
pattern ConstInt :: Int -> QuadrupleArgument
pattern ConstInt x = ConstValue (IntValue x)

pattern ConstBool :: Bool -> QuadrupleArgument
pattern ConstBool x = ConstValue (BoolValue x)

pattern ConstString :: String -> QuadrupleArgument
pattern ConstString x = ConstValue (StringValue x)

pattern QuadrupleRegister :: Type -> Index -> QuadrupleArgument
pattern QuadrupleRegister _type register = Register (TemporaryRegister _type register)
