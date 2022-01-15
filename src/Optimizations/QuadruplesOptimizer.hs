module Optimizations.QuadruplesOptimizer where

import qualified IntermediateCode.Definitions.Quadruples as Q
import qualified Optimizations.QuadruplesOptimizer.Context as C
import Optimizations.QuadruplesOptimizer.Utilities
import Errors
import Types
import Control.Monad.State
import Lens.Micro.Platform
import qualified Data.Map as Map
import Data.List
import qualified Data.Set as Set
import qualified Optimizations.Options as O
import Debug.Trace
import Control.Monad.Except
import Data.Maybe

removeDeadBlockConnections :: Q.Block -> C.Optimizer ()
removeDeadBlockConnections block = do
  let blockNumber = view Q.blockNumber block
  let nextBlocks = view Q.nextBlocks block
  let previousBlocks = view Q.previousBlocks block
  mapM_ (removeQuadrupleEdge blockNumber) nextBlocks
  mapM_ (\previousBlock -> do
          removeQuadrupleEdge previousBlock blockNumber
          modifyBlock previousBlock $ over Q.finalOperation downgradeFinalOperation
        ) previousBlocks

removeDeadBlocks :: C.Optimizer ()
removeDeadBlocks = do
  deadBlocks <- filter (not . view Q.isAlive) . Map.elems . view C.blocks <$> get
  mapM_ removeDeadBlockConnections deadBlocks
  mapM_ removeBlock $ map (view Q.blockNumber) deadBlocks

mergeBlocks' :: Q.BlockNumber -> Q.BlockNumber -> C.Optimizer ()
mergeBlocks' source destination = do
  destinationBlock <- getBlock destination
  sourceBlock <- getBlock source
  modifyBlock source $ set Q.hasReturn $ view Q.hasReturn destinationBlock
  modifyBlock source $ set Q.finalVariables $ view Q.finalVariables destinationBlock
  modifyBlock source $ set Q.finalOperation $ view Q.finalOperation destinationBlock
  removeQuadrupleEdge source destination
  mapM_ (removeQuadrupleEdge destination) $ view Q.nextBlocks destinationBlock
  mapM_ (addQuadrupleEdge source) $ reverse $ view Q.nextBlocks destinationBlock
  let finalVariables = view Q.finalVariables sourceBlock
  let phiVariables = view Q.phiVariables destinationBlock
  let destinationIdents = Map.keysSet $ view Q.phiVariables destinationBlock
  let tuples = map (\i -> let
          first = fromJust $ Map.lookup i phiVariables
          second = fromJust $ Map.lookup i finalVariables
        in
          (first, second)
        ) $ Set.toList destinationIdents
  let assigments = map (\(t, l) -> Q.QuadrupleOperation t $ Q.Assigment l) tuples
  let sourceCode = view Q.code sourceBlock
  let destinationCode = view Q.code destinationBlock
  modifyBlock source $ set Q.code (sourceCode ++ assigments ++ destinationCode)
  removeBlock destination

mergeBlocks :: C.Optimizer ()
mergeBlocks = do
  blocks <- view C.blocks <$> get
  let oneNext = Map.elems $ Map.filter ((==) 1 . length . view Q.nextBlocks) blocks
  let onePrevious = Map.keysSet $ Map.filter ((==) 1 . length . view Q.previousBlocks) blocks
  let tuples = map (\b -> (view Q.blockNumber b, head $ view Q.nextBlocks b)) oneNext
  let blocksToMerge = filter (\(_, d) -> Set.member d onePrevious) tuples
  let sortedBlocks = sortBy (flip compare) blocksToMerge
  mapM_ (uncurry mergeBlocks') sortedBlocks

optimizePhi' :: Q.Block -> C.Optimizer ()
optimizePhi' block = do
  let blockNumber = view Q.blockNumber block
  let phiVariables = view Q.phiVariables block
  previousBlocks <- mapM getBlock $  view Q.previousBlocks block
  let phis = foldl (\outer fv -> let
            foldFun = (\inner (k, v) -> Map.insertWith Set.union k (Set.singleton v) inner)
          in
            foldl foldFun outer $ Map.toList fv
        ) Map.empty $ map (view Q.finalVariables) previousBlocks
  let triples = map (\(i, r) -> (r, i, fromJust $ Map.lookup i phis)) $ Map.toList $ Map.filterWithKey (\k _ -> Map.member k phis) phiVariables
  when (length triples /= Map.size phiVariables) $ throwError $ InternalCompilerError "Cannot optimize phis"
  let redundantPhis = catMaybes $ map getRedundantPhis triples
  mapM_ (\(r, i, l) -> do
      let assigment = Q.Assigment l
      let operation = Q.QuadrupleOperation r $ Q.Assigment l
      modifyBlock blockNumber $ over Q.phiVariables (Map.delete i)
      modifyBlock blockNumber $ over Q.code (operation:)
      modifyBlock blockNumber $ over Q.requiredRegisters (Set.union $ Set.fromList $ Q.getRegisters assigment)
    ) redundantPhis
  shrinkFinalVariables block
  when (not $ null redundantPhis) setWasOptimized

optimizePhi :: C.Optimizer ()
optimizePhi = do
  blocks <- filter (view Q.isAlive) . Map.elems . view C.blocks <$> get
  mapM_ optimizePhi' blocks

copyPropagations' :: Q.Block -> C.Optimizer ()
copyPropagations' block = do
  let blockNumber = view Q.blockNumber block
  let code = view Q.code block
  maybeCode <- mapM copyReplace code
  let newCode = catMaybes maybeCode
  modifiedFinalVariables <- mapM getFirstDefinition (view Q.finalVariables block)
  modifyBlock blockNumber $ set Q.code newCode
  modifyBlock blockNumber $ set Q.finalVariables modifiedFinalVariables

copyPropagations :: C.Optimizer ()
copyPropagations = do
  blocks <- Map.elems . view C.blocks <$> get
  mapM_ copyPropagations' blocks

deadCodeElimination' :: Q.Block -> C.Optimizer ()
deadCodeElimination' block = do
  let blockNumber = view Q.blockNumber block
  initialLiveRegisters <- getInitialLiveRegisters block
  modify $ set C.liveRegisters initialLiveRegisters
  maybeCode <- reverse <$> mapM removeRedundantQuadruple (reverse $ view Q.code block)
  liveRegisters <- view C.liveRegisters <$> get
  let newCode = catMaybes maybeCode
  modifyBlock blockNumber $ set Q.code newCode
  setLiveRegisters block
  resetDeadCodeElimination
  when (any isNothing maybeCode) setWasOptimized

deadCodeElimination :: C.Optimizer ()
deadCodeElimination = do
  lastBlocks <- Map.keys . Map.filter (null . view Q.nextBlocks) . view C.blocks <$> get
  mapM_ (\b -> modifyBlock b $ set Q.finalVariables Map.empty) lastBlocks
  blocks <- reverse . Map.elems . view C.blocks <$> get
  mapM_ deadCodeElimination' blocks

gcse' :: Q.Block -> C.Optimizer ()
gcse' block = do
  initializeGCSE block
  let blockNumber = view Q.blockNumber block
  let code = view Q.code block
  newCode <- mapM (gcseReplace block) code
  modifyBlock blockNumber $ set Q.code newCode
  saveGlobalOperations blockNumber

gcse :: C.Optimizer ()
gcse = do
  blocks <- Map.elems . view C.blocks <$> get
  mapM_ gcse' blocks
  resetGCSE

optimizeFunctionLoop :: O.OptimizerOptions -> Int -> C.Optimizer ()
optimizeFunctionLoop options 0 = return ()
optimizeFunctionLoop options n = do
  when (view O.optimizePhi options) optimizePhi
  when (view O.deadCodeElimination options) deadCodeElimination
  when (view O.copyPropagations options) copyPropagations
  when (view O.gcse options) gcse
  wasOptimized <- view C.wasOptimized <$> get
  resetOptimizer
  when wasOptimized $ optimizeFunctionLoop options (n - 1)

optimizeFunction :: O.OptimizerOptions -> C.Optimizer ()
optimizeFunction options = do
  when (view O.deadBlocks options) removeDeadBlocks
  when (view O.mergeBlocks options) mergeBlocks
  calculateDominators
  optimizeFunctionLoop options $ view O.optimizerIterations options

optimizeQuadruples :: O.OptimizerOptions -> Q.Quadruples -> Either CompilerError Q.Quadruples
optimizeQuadruples options quadruples = case runQuadrupleOptimizer options quadruples of
  Left latteError -> Left $ CompilerError latteError NoPosition
  Right optimizedQuadruples -> Right optimizedQuadruples
-- 
-- Runners
-- 
convertOptimizedFunctions :: C.OptimizerContext -> Q.FunctionDefinition
convertOptimizedFunctions context = let
    functionDefinition = view C.functionDefiniton context
    blocks = view C.blocks context
    registerCounter = view C.registerCounter context
  in
    set Q.registerCounter registerCounter $ set Q.blocks blocks functionDefinition


runQuadrupleOptimizer :: O.OptimizerOptions -> Q.Quadruples -> Either LatteError Q.Quadruples
runQuadrupleOptimizer options quadruples = do 
  let functions = view Q.functions quadruples
  let optimizer = optimizeFunction options
  functionsResult <- mapM (fmap snd . runStateT optimizer . C.emptyOptimizerContext) functions
  let optimizedFunctions = Map.map (convertOptimizedFunctions) functionsResult
  return $ Q.Quadruples optimizedFunctions