{-# LANGUAGE TemplateHaskell #-}
module Optimizations.QuadruplesOptimizer.Context where

import qualified IntermediateCode.Definitions.Quadruples as Q
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lens.Micro.Platform
import Errors
import Control.Monad.State


type LocalOperations = Map.Map Q.QuadrupleOperation Q.TemporaryRegister
type GlobalOperations = Map.Map Q.QuadrupleOperation Q.TemporaryRegister

data OptimizerContext
  = OptimizerContext {
    _functionDefiniton :: Q.FunctionDefinition,
    _registerCounter :: Int,
    _blocks :: Map.Map Q.BlockNumber Q.Block,
    _assigments :: Map.Map Q.TemporaryRegister Q.QuadrupleLocation,
    _wasOptimized :: Bool,
    _localOperations :: Map.Map Q.BlockNumber LocalOperations,
    _globalOperations :: Map.Map Q.BlockNumber GlobalOperations,
    _dominators :: Map.Map Q.BlockNumber (Set.Set Q.BlockNumber),
    _liveRegisters :: Set.Set Q.TemporaryRegister
  }

$(makeLenses ''OptimizerContext)

type Optimizer = StateT OptimizerContext (Either LatteError)

emptyOptimizerContext :: Q.FunctionDefinition -> OptimizerContext
emptyOptimizerContext __functionDefinition = let
    __blocks = view Q.blocks __functionDefinition
    __regitsterCounter = view Q.registerCounter __functionDefinition
  in
    OptimizerContext __functionDefinition __regitsterCounter __blocks Map.empty False Map.empty Map.empty Map.empty Set.empty