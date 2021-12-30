{-# LANGUAGE TemplateHaskell #-}
module IntermediateCode.TransformerContext where

import qualified Data.Map as Map
import Control.Monad.State
import Errors
import Types
import IntermediateCode.Definitions.Quadruples
    ( QuadrupleResult,
      QuadruplesCode, 
      Quadruple, 
      emptyQuadruplesCode )
import Lens.Micro.Platform


type GlobalContext = QuadruplesCode 
data FunctionContext 
  = FunctionContext {
    _functionName :: Ident, 
    _blockCounter :: Int,
    _counter :: Int,
    _returnType :: Type,
    _arguments  :: [Argument],
    _scope :: [Ident],
    _variables :: Map.Map Ident [Type],
    _resultTypes :: Map.Map QuadrupleResult Type,
    _currentBlock :: [Quadruple],
    _blocks :: Map.Map Int [Quadruple]
  }

$(makeLenses ''FunctionContext)

type GlobalTransformer = StateT GlobalContext (Either LatteError)
type FunctionTransformer = StateT FunctionContext (StateT QuadruplesCode (Either LatteError))


emptyGlobalContext :: QuadruplesCode
emptyGlobalContext = emptyQuadruplesCode

emptyFunctionContext :: Type -> Ident -> [Argument] -> FunctionContext
emptyFunctionContext retType ident args =
  FunctionContext ident 0 0 retType args [] Map.empty Map.empty [] Map.empty