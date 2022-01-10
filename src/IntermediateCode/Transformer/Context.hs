{-# LANGUAGE TemplateHaskell #-}
module IntermediateCode.Transformer.Context where

import qualified Data.Map as Map
import Control.Monad.State
import Errors
import Types
import qualified IntermediateCode.Definitions.Quadruples as Q
import Lens.Micro.Platform
import Debug.Trace
import Data.List



data PreQuadruple
  = Quadruple Q.Quadruple
  | PhiPlaceholder Ident Q.TemporaryRegister
  | JumpPlaceholder
  | ConditionalJumpPlaceholder Q.QuadrupleLocation
  deriving (Show)

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
    _blockCounter :: Q.BlockNumber,
    _registerCounter :: Index,
    _dummyCounter :: Index,
    _scopes :: [[Ident]],
    _variables :: Map.Map Ident [Q.QuadrupleLocation],
    _currentBlockNumber :: Maybe Q.BlockNumber,
    _blocks :: Map.Map Q.BlockNumber BlockContext,
    _finalBlocks :: [Q.BlockNumber]
  }

data GlobalContext 
  = GlobalContext {
    _quadruples :: Q.Quadruples,
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
    FunctionContext ident retType args 0 1 0 [] Map.empty Nothing Map.empty []

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