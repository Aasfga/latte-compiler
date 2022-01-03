{-# LANGUAGE TemplateHaskell #-}
module IntermediateCode.Definitions.Quadruples where

import Lens.Micro.Platform hiding (element)
import qualified Data.Map as Map
import Types




-- Definitions
type TemporaryRegister = Int
type BlockNumber = Int

data QuadrupleArgument
  = TemporaryRegister TemporaryRegister
  | Value Value

data Quadruple 
  = QuadrupleOperation TemporaryRegister QuadrupleOperation

data QuadrupleOperation
  = ARG_INIT Int Type

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


-- Functions
emptyQuadruplesCode :: QuadruplesCode
emptyQuadruplesCode =
  let 
    functions = Map.empty
  in
    QuadruplesCode functions

emptyFunction :: Type -> [Argument] -> FunctionCode
emptyFunction retType args = FunctionCode Map.empty retType args

getOperationType :: QuadrupleOperation -> Type
getOperationType (ARG_INIT _ _type) = _type