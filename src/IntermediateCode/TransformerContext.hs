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

type Location 
  = QuadrupleArgument

data BlockContext
  = BlockContext {
    _blockNumber :: Int,
    _finalVariables :: Map.Map Ident QuadrupleArgument,
    _previousBlocks :: [BlockNumber],
    _nextBlocks :: [BlockNumber],
    _isAlive :: Bool,
    _code :: [PreQuadruple]
  }

data FunctionContext 
  = FunctionContext {
    _functionName :: Ident, 
    _returnType :: Type,
    _arguments  :: [Argument],
    _blockCounter :: Int,
    _registerCounter :: Int,
    _scopes :: [[Ident]],
    _variables :: Map.Map Ident [QuadrupleArgument],
    _currentBlockNumber :: Maybe BlockNumber,
    _blocks :: Map.Map Int BlockContext
  }

type GlobalContext = QuadruplesCode 

$(makeLenses ''BlockContext)
$(makeLenses ''FunctionContext)
-- $(makeLenses ''VariableInfo)

type GlobalTransformer = StateT GlobalContext (Either LatteError)
type FunctionTransformer = StateT FunctionContext GlobalTransformer

emptyGlobalContext :: QuadruplesCode
emptyGlobalContext = emptyQuadruplesCode

emptyBlockContext :: BlockNumber -> BlockContext
emptyBlockContext block = BlockContext block Map.empty [] [] True []

emptyFunctionContext :: Type -> Ident -> [Argument] -> FunctionContext
emptyFunctionContext retType ident args =
    FunctionContext ident retType args 0 0 [] Map.empty Nothing Map.empty

getLocationType :: Location -> Type
getLocationType = getQuadrupleArgumentType