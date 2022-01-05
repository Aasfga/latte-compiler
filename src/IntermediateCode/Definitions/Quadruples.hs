{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module IntermediateCode.Definitions.Quadruples where

import Lens.Micro.Platform hiding (element)
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
  = ArgInit Int Type
  | IntegerAdd QuadrupleArgument QuadrupleArgument
  | IntegerSub QuadrupleArgument QuadrupleArgument
  | ReturnValue QuadrupleArgument
  | ReturnVoid

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
emptyQuadruplesCode =
  let 
    functions = Map.empty
  in
    QuadruplesCode functions

emptyFunction :: Type -> [Argument] -> FunctionCode
emptyFunction retType args = FunctionCode Map.empty retType args

getOperationType :: QuadrupleOperation -> Type
getOperationType (ArgInit _ _type) = _type

getQuadrupleArgumentType :: QuadrupleArgument -> Type
getQuadrupleArgumentType (QuadrupleRegister _type _) = _type
getQuadrupleArgumentType (ConstInt _) = Int
getQuadrupleArgumentType (ConstString _) = String
getQuadrupleArgumentType (ConstBool _) = Bool
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
