{-# LANGUAGE TemplateHaskell #-}
module IntermediateCode.Transformer.Context where

import qualified Data.Map as Map
import Control.Monad.State
import Errors
import Types
import qualified IntermediateCode.Definitions.Quadruples as Q
import Lens.Micro.Platform
import Debug.Trace



data PreQuadruple
  = Quadruple Q.Quadruple
  | PhiPlaceholder Ident
  | JumpPlaceholder
  | ConditionalJumpPlaceholder Q.QuadrupleLocation

data BlockContext
  = BlockContext {
    _blockNumber :: Int,
    _finalVariables :: Map.Map Ident Q.QuadrupleLocation,
    _previousBlocks :: [Q.BlockNumber],
    _nextBlocks :: [Q.BlockNumber],
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
    _variables :: Map.Map Ident [Q.QuadrupleLocation],
    _currentBlockNumber :: Maybe Q.BlockNumber,
    _blocks :: Map.Map Int BlockContext,
    _finalBlocks :: [Q.BlockNumber]
  }

data GlobalContext 
  = GlobalContext {
    _quadruplesCode :: Q.QuadruplesCode,
    _position :: Position
  }

$(makeLenses ''BlockContext)
$(makeLenses ''FunctionContext)
$(makeLenses ''GlobalContext)

type GlobalTransformer = StateT GlobalContext (Either (LatteError, Position))
type FunctionTransformer = StateT FunctionContext GlobalTransformer

emptyGlobalContext :: GlobalContext 
emptyGlobalContext = GlobalContext Q.emptyQuadruplesCode NoPosition

emptyBlockContext :: Q.BlockNumber -> Bool -> BlockContext
emptyBlockContext block isAlive = BlockContext block Map.empty [] [] isAlive [] False

emptyFunctionContext :: Type -> Ident -> [Argument] -> FunctionContext
emptyFunctionContext retType ident args =
    FunctionContext ident retType args 0 0 [] Map.empty Nothing Map.empty []

instance Show BlockContext where
  show block = let
      number = view blockNumber block
      alive = view isAlive block
      nb = view nextBlocks block
      pb = view previousBlocks block
      return = view hasReturn block
    in
      "number=" ++ show number ++ " alive=" ++ show alive ++ " hasReturn=" ++ show return ++ " nextBlocks=" ++ show nb ++ " previousBlocks=" ++ show pb