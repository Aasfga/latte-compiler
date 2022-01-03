{-# LANGUAGE TemplateHaskell #-}
module IntermediateCode.TransformerContext where

import qualified Data.Map as Map
import Control.Monad.State
import Errors
import Types
import IntermediateCode.Definitions.Quadruples as Q
import Lens.Micro.Platform



data PreQuadruple
  = Quadruple Quadruple
  | PhiPlaceholder Ident

type VariableLocation 
  = QuadrupleArgument

data VariableInfo 
  = VariableInfo {
    _variableType :: Type,
    _location :: VariableLocation 
  }

data BlockContext
  = BlockContext {
    _blockNumber :: Int,
    _variablesLocation :: Map.Map Ident VariableLocation,
    _previousBlocks :: [BlockNumber],
    _nextBlocks :: [BlockNumber],
    _code :: [PreQuadruple]
  }

data FunctionContext 
  = FunctionContext {
    _functionName :: Ident, 
    _returnType :: Type,
    _arguments  :: [Argument],
    _blockCounter :: Int,
    _registerCounter :: Int,
    _scope :: [[Ident]],
    _variables :: Map.Map Ident [VariableInfo],
    _resultTypes :: Map.Map TemporaryRegister Type,
    _currentBlockNumber :: BlockNumber,
    _blocks :: Map.Map Int BlockContext
  }

type GlobalContext = QuadruplesCode 

$(makeLenses ''BlockContext)
$(makeLenses ''FunctionContext)
$(makeLenses ''VariableInfo)

type GlobalTransformer = StateT GlobalContext (Either LatteError)
type FunctionTransformer = StateT FunctionContext GlobalTransformer

emptyGlobalContext :: QuadruplesCode
emptyGlobalContext = emptyQuadruplesCode

emptyBlockContext :: BlockNumber -> BlockContext
emptyBlockContext  block = BlockContext block Map.empty [] [] []

emptyFunctionContext :: Type -> Ident -> [Argument] -> FunctionContext
emptyFunctionContext retType ident args =
    FunctionContext ident retType args 0 0 [] Map.empty Map.empty 0 Map.empty