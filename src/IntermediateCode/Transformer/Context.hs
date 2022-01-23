{-# LANGUAGE TemplateHaskell #-}
module IntermediateCode.Transformer.Context where

import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import Control.Monad.State
import Errors
import Types
import qualified IntermediateCode.Definitions.Quadruples as Q
import Lens.Micro.Platform
import Debug.Trace
import Data.List



data PreQuadruple
  = Quadruple Q.Quadruple
  deriving (Show)

data BlockContext
  = BlockContext {
    _blockNumber :: Int,
    _phiVariables :: Map.Map Ident Q.TemporaryRegister,
    _finalVariables :: Map.Map Ident Q.QuadrupleLocation,
    _previousBlocks :: [Q.BlockNumber],
    _nextBlocks :: [Q.BlockNumber],
    _isAlive :: Bool,
    _code :: [PreQuadruple],
    _hasReturn :: Bool,
    _finalOperation :: Q.FinalOperation
  }

data FunctionContext 
  = FunctionContext {
    _functionIdent :: Ident, 
    _returnType :: Type,
    _arguments  :: [Argument],
    _blockCounter :: Q.BlockNumber,
    _registerCounter :: Index,
    _dummyCounter :: Index,
    _scopes :: [[Ident]],
    _variables :: Map.Map Ident [Q.QuadrupleLocation],
    _locationTypes :: Map.Map Q.TemporaryRegister Type,
    _currentBlockNumber :: Maybe Q.BlockNumber,
    _blocks :: Map.Map Q.BlockNumber BlockContext,
    _finalBlocks :: [Q.BlockNumber]
  }

data ClassDefinition
  = ClassDefinition {
    _attributes :: Map.Map Ident Index,
    _attributeTypes :: Map.Map Ident Type
  }

data GlobalContext 
  = GlobalContext {
    _quadruples :: Q.Quadruples,
    _classes :: Map.Map Ident ClassDefinition,
    _position :: Position
  }

$(makeLenses ''BlockContext)
$(makeLenses ''ClassDefinition)
$(makeLenses ''FunctionContext)
$(makeLenses ''GlobalContext)

type GlobalTransformer = StateT GlobalContext (Either (LatteError, Position))
type FunctionTransformer = StateT FunctionContext GlobalTransformer

emptyGlobalContext :: GlobalContext 
emptyGlobalContext = GlobalContext Q.emptyQuadruplesCode Map.empty NoPosition

emptyBlockContext :: Q.BlockNumber -> Bool -> (Map.Map Ident Q.TemporaryRegister) -> BlockContext
emptyBlockContext __blockNumber __isAlive __phiVariables = BlockContext __blockNumber __phiVariables Map.empty [] [] __isAlive [] False Q.None

emptyFunctionContext :: Type -> Ident -> [Argument] -> FunctionContext
emptyFunctionContext retType ident args =
    FunctionContext ident retType args 0 1 0 [] Map.empty Map.empty Nothing Map.empty []

instance Show BlockContext where
  show block = let
      number = view blockNumber block
      alive = view isAlive block
      nb = view nextBlocks block
      pb = view previousBlocks block
      return = view hasReturn block
      fv = view finalVariables block
      tuples = Map.toList fv
      c = concat $ intersperse "\n" $ map show $ reverse $ view code block
    in
      "Block " ++ show number ++ ": \n" ++
      " alive=" ++ show alive ++ "\n" ++ 
      " hasReturn=" ++ show return ++ "\n" ++ 
      " nextBlocks=" ++ show nb ++ "\n" ++ 
      " previousBlocks=" ++ show pb ++ "\n" ++ 
      " finalVariables=" ++ show tuples ++ "\n" ++
      " code= \n" ++ c