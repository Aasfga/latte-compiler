module Optimizations.QuadruplesOptimizer.Utilities where

import qualified IntermediateCode.Definitions.Quadruples as Q
import qualified Optimizations.QuadruplesOptimizer.Context as C
import Errors
import Control.Monad.State
import Lens.Micro.Platform
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad.Except
import Types
import Debug.Trace
-- 
-- Block utilities
-- 
getNewRegister :: C.Optimizer Q.TemporaryRegister
getNewRegister = do
  newRegister <- view C.registerCounter <$> get
  modify $ over C.registerCounter (+1)
  return newRegister

addQuadrupleEdge :: Q.BlockNumber -> Q.BlockNumber -> C.Optimizer ()
addQuadrupleEdge source destination = do
  modifyBlock source $ over Q.nextBlocks (destination:)
  modifyBlock destination $ over Q.previousBlocks (source:)

removeQuadrupleEdge :: Q.BlockNumber -> Q.BlockNumber -> C.Optimizer ()
removeQuadrupleEdge source destination = do
  modifyBlock source $ over Q.nextBlocks (filter (destination /=))
  modifyBlock destination $ over Q.previousBlocks (filter (source /=))


downgradeFinalOperation :: Q.FinalOperation -> Q.FinalOperation
downgradeFinalOperation Q.Jump = Q.None
downgradeFinalOperation (Q.ConditionalJump _) = Q.Jump
downgradeFinalOperation (Q.Return) = Q.Return
downgradeFinalOperation (Q.None) = Q.None

resetOptimizer :: C.Optimizer ()
resetOptimizer = do
  modify $ set C.assigments Map.empty
  modify $ set C.wasOptimized False
  modify $ set C.localOperations Map.empty

setWasOptimized :: C.Optimizer ()
setWasOptimized = modify $ set C.wasOptimized True
-- 
-- Getters, Setters and Modifiers
-- 
getBlock :: Q.BlockNumber -> C.Optimizer Q.Block
getBlock blockNumber = do
  maybeBlock <- Map.lookup blockNumber . view C.blocks <$> get
  case maybeBlock of 
    Nothing -> throwError $ InternalCompilerError "Not able to find block with given block number"
    Just block -> return block

modifyBlock :: Q.BlockNumber -> (Q.Block -> Q.Block) -> C.Optimizer ()
modifyBlock blockNumber f = do
  isMember <- Map.member blockNumber . view C.blocks <$> get
  case isMember of
    False -> throwError $ InternalCompilerError "Not able to find block with given block number"
    True -> modify $ over C.blocks (Map.update (Just . f) blockNumber)

removeBlock :: Q.BlockNumber -> C.Optimizer ()
removeBlock blockNumber = do
  modify $ over C.blocks $ Map.delete blockNumber
-- 
-- Phi elimination
-- 
getRedundantPhis :: (Q.TemporaryRegister, Ident, Set.Set Q.QuadrupleLocation) -> Maybe (Q.TemporaryRegister, Ident, Q.QuadrupleLocation)
getRedundantPhis (register, ident, locationSet)
  | Set.size locationSet == 1 = Just (register, ident, Set.findMin locationSet)
  | Set.size locationSet == 2 && Set.member (Q.Register register) locationSet = Just (register, ident, Set.findMin $ Set.filter (/= (Q.Register register)) locationSet)
  | otherwise = Nothing

shrinkFinalVariables :: Q.Block -> C.Optimizer ()
shrinkFinalVariables block = do
  let blockNumber = view Q.blockNumber block
  nextBlocks <- mapM getBlock $ view Q.nextBlocks block
  let allPhiIdents = Set.unions $ map (Map.keysSet . view Q.phiVariables) nextBlocks
  let finalVariables = view Q.finalVariables block
  let filteredFinalVariables = Map.filterWithKey (\i _ -> Set.member i allPhiIdents) finalVariables
  modifyBlock blockNumber $ set Q.finalVariables filteredFinalVariables
-- 
-- Copy propagations
-- 
getFirstDefinition :: Q.QuadrupleLocation -> C.Optimizer Q.QuadrupleLocation
getFirstDefinition register@(Q.Register temporaryRegister) = do
  assigments <- view C.assigments <$> get
  maybeLocation <- Map.lookup temporaryRegister . view C.assigments <$> get
  case maybeLocation of
    Nothing -> return register
    Just betterRegister -> do 
      setWasOptimized
      getFirstDefinition betterRegister 
getFirstDefinition const = return const

type UnaryQuadupleConstructor = Q.QuadrupleLocation -> Q.QuadrupleOperation
type BinaryQuadrupleConstructor = Q.QuadrupleLocation -> Q.QuadrupleLocation -> Q.QuadrupleOperation
type CompareQuadrupleConstructor = Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> Q.QuadrupleOperation

replaceUnaryConstructor :: UnaryQuadupleConstructor -> Q.TemporaryRegister -> Q.QuadrupleLocation -> C.Optimizer Q.Quadruple
replaceUnaryConstructor constructor register location = do
  newLocation <- getFirstDefinition location
  return $ Q.QuadrupleOperation register $ constructor newLocation

replaceBinaryConstructor :: BinaryQuadrupleConstructor -> Q.TemporaryRegister -> Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.Optimizer Q.Quadruple
replaceBinaryConstructor constructor register first second = do
    newFirst <- getFirstDefinition first
    newSecond <- getFirstDefinition second
    return $ Q.QuadrupleOperation register $ constructor newFirst newSecond

replaceCompare :: CompareQuadrupleConstructor -> Q.TemporaryRegister -> CompareOperation -> Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.Optimizer Q.Quadruple
replaceCompare constructor register op = replaceBinaryConstructor (\x y -> constructor x op y) register

copyReplace :: Q.Quadruple -> C.Optimizer (Maybe Q.Quadruple)
copyReplace all@(Q.QuadrupleOperation register operation) = case operation of
  (Q.ArgumentInit _) -> return $ Just all
  (Q.Assigment location) -> do
    firstDefinition <- getFirstDefinition location
    modify $ over C.assigments (Map.insert register firstDefinition)
    return Nothing
  (Q.IntegerAdd first second) -> Just <$> replaceBinaryConstructor Q.IntegerAdd register first second
  (Q.IntegerSub first second) -> Just <$> replaceBinaryConstructor Q.IntegerSub register first second
  (Q.IntegerMul first second) -> Just <$> replaceBinaryConstructor Q.IntegerMul register first second
  (Q.IntegerDiv first second) -> Just <$> replaceBinaryConstructor Q.IntegerDiv register first second
  (Q.IntegerMod first second) -> Just <$> replaceBinaryConstructor Q.IntegerMod register first second
  (Q.BoolAnd first second) -> Just <$> replaceBinaryConstructor Q.BoolAnd register first second
  (Q.BoolOr first second) -> Just <$> replaceBinaryConstructor Q.BoolOr register first second
  (Q.BoolNot location) -> Just <$> replaceUnaryConstructor Q.BoolNot register location
  (Q.StringConcat first second) -> Just <$> replaceBinaryConstructor Q.StringConcat register first second
  (Q.IntegerCompare first op second) -> Just <$> replaceCompare Q.IntegerCompare register op first second
  (Q.BoolCompare first op second) -> Just <$> replaceCompare Q.BoolCompare register op first second
  (Q.StringCompare first op second) -> Just <$> replaceCompare Q.StringCompare register op first second
  (Q.ReturnValue location) -> Just <$> replaceUnaryConstructor Q.ReturnValue register location
  (Q.ReturnVoid) -> return $ Just $ all
  (Q.CallFunction ident locations) -> do
    newLocations <- mapM getFirstDefinition locations
    return $ Just $ Q.QuadrupleOperation register $ Q.CallFunction ident newLocations
-- 
-- GCSE
-- 
getProperDominators :: Q.BlockNumber -> C.Optimizer (Set.Set Q.BlockNumber)
getProperDominators blockNumber = do
  dominators <- getDominators blockNumber
  return $ Set.delete blockNumber dominators

getDominators :: Q.BlockNumber -> C.Optimizer (Set.Set Q.BlockNumber)
getDominators blockNumber = do
  maybeDominators <- Map.lookup blockNumber . view C.dominators <$> get
  case maybeDominators of
    Nothing -> throwError $ InternalCompilerError "Not able to find dominators for given block"
    Just dominators -> return dominators

setDominators :: Q.BlockNumber -> Set.Set Q.BlockNumber-> C.Optimizer ()
setDominators blockNumber dominators = modify $ over C.dominators (Map.insert blockNumber dominators)

calculateDominators'' :: Q.BlockNumber -> C.Optimizer Bool
calculateDominators'' blockNumber = do
  block <- getBlock blockNumber
  currentDominators <- getDominators blockNumber
  previousDominators <- mapM getDominators $ view Q.previousBlocks block
  case previousDominators of
    [] -> return False
    otherwise -> do
      let intersection = foldl1 Set.intersection previousDominators
      let newDominators = Set.insert blockNumber intersection
      setDominators blockNumber newDominators
      return $ currentDominators /= newDominators

calculateDominators' :: C.Optimizer ()
calculateDominators' = do
  blocks <- Map.keysSet . view C.blocks <$> get
  let blockList = Set.toList blocks
  results <- mapM calculateDominators'' blockList
  case any id results of
    False -> return ()
    True -> calculateDominators'

calculateDominators :: C.Optimizer ()
calculateDominators = do
  blocks <- Map.keysSet . view C.blocks <$> get
  let blockList = Set.toList blocks
  let root = minimum blocks
  mapM (\b -> modify $ over C.dominators (Map.insert b blocks)) blockList
  modify $ over C.dominators (Map.insert root $ Set.singleton root)
  calculateDominators'


showDominators' :: Q.BlockNumber -> C.Optimizer ()
showDominators' blockNumber = do
  dominators <- getDominators blockNumber
  let msg = show blockNumber ++ ": " ++ show (Set.toList dominators)
  when (trace msg False) $ throwError $ InternalCompilerError "xyz"

showDominators :: C.Optimizer ()
showDominators = do
  blocks <- Map.keys . view C.blocks <$> get
  mapM_ showDominators' blocks

resetGCSE :: C.Optimizer ()
resetGCSE = do
  modify $ set C.globalOperations Map.empty
  modify $ set C.localOperations Map.empty

modifyLocalOperations :: Q.BlockNumber -> (C.LocalOperations -> C.LocalOperations) -> C.Optimizer ()
modifyLocalOperations blockNumber f = modify $ over C.localOperations (Map.update (Just . f) blockNumber)

modifyGlobalOperations :: Q.BlockNumber -> (C.GlobalOperations -> C.GlobalOperations) -> C.Optimizer ()
modifyGlobalOperations blockNumber f = modify $ over C.globalOperations (Map.update (Just . f) blockNumber)

getLocalOperations :: Q.BlockNumber -> C.Optimizer C.LocalOperations
getLocalOperations blockNumber = do
  maybeResult <- Map.lookup blockNumber . view C.localOperations <$> get
  case maybeResult of 
    Nothing -> throwError $ InternalCompilerError "Not able to find local operations for given block"
    Just x -> return x

getGlobalOperations :: Q.BlockNumber -> C.Optimizer C.GlobalOperations
getGlobalOperations blockNumber = do
  maybeResult <- Map.lookup blockNumber . view C.globalOperations <$> get
  case maybeResult of 
    Nothing -> throwError $ InternalCompilerError "Not able to find global operations for given block"
    Just x -> return x

initializeGCSE :: Q.Block -> C.Optimizer ()
initializeGCSE block = do
  let blockNumber = view Q.blockNumber block
  let previousBlocks = filter ((>) blockNumber) $ view Q.previousBlocks block
  previousGlobalOperations <- mapM getGlobalOperations previousBlocks
  let globalOperations = foldl Map.union Map.empty previousGlobalOperations
  modify $ over C.localOperations $ Map.insert blockNumber Map.empty
  modify $ over C.globalOperations $ Map.insert blockNumber globalOperations

saveGlobalOperations :: Q.BlockNumber -> C.Optimizer ()
saveGlobalOperations blockNumber = do
  localOperations <- getLocalOperations blockNumber
  modifyGlobalOperations blockNumber (Map.union localOperations)

addRegisterToRequiredRegisters :: Q.QuadrupleOperation -> Q.TemporaryRegister -> Q.BlockNumber -> C.Optimizer ()
addRegisterToRequiredRegisters operation temporaryRegister blockNumber = do
  localOperations <- getLocalOperations blockNumber
  case Map.member operation localOperations of
    True -> return ()
    False -> do
      modifyBlock blockNumber $ over Q.requiredRegisters (Set.insert temporaryRegister)
      previousBlocks <- filter ((>)blockNumber) . view Q.previousBlocks <$> getBlock blockNumber
      mapM_ (addRegisterToRequiredRegisters operation temporaryRegister) previousBlocks 

saveOperationInDominator :: Q.BlockNumber -> Q.QuadrupleOperation -> C.Optimizer Q.TemporaryRegister
saveOperationInDominator blockNumber operation = do
  dominator <- maximum <$> getProperDominators blockNumber
  newRegister <- getNewRegister
  let quadruple = Q.QuadrupleOperation newRegister operation
  modifyBlock dominator $ over Q.code (\c -> c ++ [quadruple])
  let registers = Q.getRegisters operation
  mapM (\r -> addRegisterToRequiredRegisters operation r dominator) registers
  return newRegister

isGcseReplaceble :: Q.QuadrupleOperation -> Bool
isGcseReplaceble (Q.ArgumentInit _) = False
isGcseReplaceble (Q.Assigment _) = False
isGcseReplaceble (Q.ReturnValue _) = False
isGcseReplaceble (Q.ReturnVoid) = False
isGcseReplaceble (Q.CallFunction "debugGcsePrintInt" _) = True
isGcseReplaceble (Q.CallFunction _ _) = False
isGcseReplaceble _ = True

gcseReplace :: Q.Block -> Q.Quadruple -> C.Optimizer Q.Quadruple
gcseReplace block all@(Q.QuadrupleOperation register operation)
  | not $ isGcseReplaceble operation = return all
  | otherwise = do
    let blockNumber = view Q.blockNumber block
    localOperations <- Map.toList . view C.localOperations <$> get
    maybeLocalRegister <- Map.lookup operation <$> getLocalOperations blockNumber
    case maybeLocalRegister of
      Nothing -> do
        maybeGlobalRegister <- Map.lookup operation <$> getGlobalOperations blockNumber
        case maybeGlobalRegister of
          Nothing -> do 
            modifyLocalOperations blockNumber $ Map.insert operation register
            return all
          Just _ -> do
            dominatorRegister <- saveOperationInDominator blockNumber operation
            addRegisterToRequiredRegisters operation dominatorRegister blockNumber
            modifyLocalOperations blockNumber $ Map.insert operation dominatorRegister
            setWasOptimized
            return $ Q.QuadrupleOperation register $ Q.Assigment $ Q.Register dominatorRegister
      Just localRegister -> do
        setWasOptimized
        return $ Q.QuadrupleOperation register $ Q.Assigment $ Q.Register localRegister
-- 
-- Dead code elimination
-- 

resetDeadCodeElimination :: C.Optimizer ()
resetDeadCodeElimination = modify $ set C.liveRegisters Set.empty

getInitialLiveRegisters :: Q.Block -> C.Optimizer (Set.Set Q.TemporaryRegister)
getInitialLiveRegisters block = do
  let finalVariables = view Q.finalVariables block
  let finalRegisters = Set.fromList $ catMaybes $ map Q.getRegister $ Map.elems finalVariables
  nextBlocks <- mapM getBlock $ view Q.nextBlocks block
  let requiredRegisters = Set.unions $ map (view Q.requiredRegisters) nextBlocks
  let finalOperation = view Q.finalOperation block
  case finalOperation of 
    Q.ConditionalJump location -> do
      let finalOperationRegisters = Set.fromList $ catMaybes $ map Q.getRegister [location]
      return $ Set.unions [finalRegisters, requiredRegisters, finalOperationRegisters]
    otherwise -> return $ Set.union finalRegisters requiredRegisters

setLiveRegisters :: Q.Block -> C.Optimizer ()
setLiveRegisters block = do
  let blockNumber = view Q.blockNumber block
  liveRegisters <- view C.liveRegisters <$> get
  let phiVariables = view Q.phiVariables block
  let phiRegisters = Set.fromList $ Map.elems phiVariables
  let filteredLiveRegisters = Set.filter (\r -> not $ Set.member r phiRegisters) liveRegisters
  let filteredPhiRegisters = Map.filter (\r -> Set.member r liveRegisters) phiVariables
  modifyBlock blockNumber $ set Q.requiredRegisters filteredLiveRegisters 
  modifyBlock blockNumber $ set Q.phiVariables filteredPhiRegisters

removeRedundantQuadruple :: Q.Quadruple -> C.Optimizer (Maybe Q.Quadruple)
removeRedundantQuadruple all@(Q.QuadrupleOperation register operation) = case operation of
  (Q.ArgumentInit _) -> do
    modify $ over C.liveRegisters $ Set.delete register
    return $ Just all
  (Q.ReturnVoid) -> return $ Just all
  (Q.ReturnValue location) -> do
    let newRegisters = Set.fromList $ catMaybes $ map Q.getRegister [location]
    modify $ over C.liveRegisters $ Set.union newRegisters
    return $ Just all
  (Q.CallFunction _ locations) -> do
    let newRegisters = Set.fromList $ catMaybes $ map Q.getRegister locations
    modify $ over C.liveRegisters $ Set.delete register
    modify $ over C.liveRegisters $ Set.union newRegisters
    return $ Just all
  otherwise -> do
      liveRegisters <- view C.liveRegisters <$> get
      case Set.member register liveRegisters of
        False -> return Nothing
        True -> do
          let newRegisters = Set.fromList $ Q.getRegisters operation
          modify $ over C.liveRegisters $ Set.union newRegisters
          modify $ over C.liveRegisters $ Set.delete register
          return $ Just all