{-# LANGUAGE TemplateHaskell #-}
module IntermediateCode.Definitions.Quadruples where

import Lens.Micro.Platform hiding (element)
import qualified Data.Map as Map
import Types




-- Definitions

type QuadrupleResult = Int

data Quadruple = Quadruple QuadrupleResult QuadrupleOperation

data QuadrupleOperation
  = NOP

data QuadruplesCode 
  = QuadruplesCode {
    _functions :: Map.Map String FunctionCode
  }

data FunctionCode
  = FunctionCode {
    _blocks :: Map.Map Label [Quadruple],
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