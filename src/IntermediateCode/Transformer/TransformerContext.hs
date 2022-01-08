{-# LANGUAGE TemplateHaskell #-}
module IntermediateCode.Transformer.TransformerContext where

import qualified Data.Map as Map
import Control.Monad.State
import Errors
import Types
import IntermediateCode.Definitions.Quadruples as Q
import Lens.Micro.Platform



data PreQuadruple
  = Quadruple Quadruple
  | PhiPlaceholder Ident
  | JumpPlaceholder
  | ConditionalJumpPlaceholder Location

type Location 
  = QuadrupleArgument

data BlockContext
  = BlockContext {
    _blockNumber :: Int,
    _finalVariables :: Map.Map Ident QuadrupleArgument,
    _previousBlocks :: [BlockNumber],
    _nextBlocks :: [BlockNumber],
    _isAlive :: Bool,
    _code :: [PreQuadruple],
    _hasReturn :: Bool
  }

data FunctionContext 
  = FunctionContext {
    _functionIdent :: Ident, 
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

emptyBlockContext :: BlockNumber -> Bool -> BlockContext
emptyBlockContext block isAlive = BlockContext block Map.empty [] [] isAlive [] False

emptyFunctionContext :: Type -> Ident -> [Argument] -> FunctionContext
emptyFunctionContext retType ident args =
    FunctionContext ident retType args 0 0 [] Map.empty Nothing Map.empty

getLocationType :: Location -> Type
getLocationType = getQuadrupleArgumentType