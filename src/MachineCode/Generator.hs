module MachineCode.Generator where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Errors
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Types
import qualified IntermediateCode.Definitions.Quadruples as Q 
import qualified MachineCode.Generator.Context as C
import Lens.Micro.Platform
import Debug.Trace
import MachineCode.Generator.Utilities
import MachineCode.Types
import MachineCode.Generator.Instructions

generateCompare :: Q.TemporaryRegister -> CompareOperation -> C.FunctionGenerator ()
generateCompare tmpRegister op = do
  elseLabel <- getNewLabel
  endLabel <- getNewLabel
  let notOp = negateCompareOperation op
  lift $ emitJXX notOp elseLabel
  lift $ emitMOV RAX (Number 1)
  lift $ emitJMP endLabel
  lift $ emitLabel elseLabel
  lift $ emitMOV RAX (Number 0)
  lift $ emitLabel endLabel
  saveValue tmpRegister RAX

generatePhi :: Q.TemporaryRegister -> (Q.QuadrupleLocation, Q.BlockNumber) -> C.FunctionGenerator ()
generatePhi tmpRegister (location, blockNumber) = do
  lift $ emitCMP R8 (Number blockNumber)
  label <- getNewLabel
  lift $ emitJXX NE label
  loadValue location RAX
  saveValue tmpRegister RAX
  lift $ emitLabel label

generateQuadrupleOperation :: Q.TemporaryRegister -> Q.QuadrupleOperation -> C.FunctionGenerator ()
generateQuadrupleOperation tmpRegister (Q.ArgumentInit index _) = do
  loadArgument index
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.Phi choices) = do
  mapM_ (generatePhi tmpRegister) choices
generateQuadrupleOperation tmpRegister (Q.IntegerAdd first second) = do 
  loadValue first RAX
  loadValue second RCX
  lift $ emitADD RAX RCX
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.IntegerSub first second) = do
  loadValue first RAX
  loadValue second RCX
  lift $ emitSUB RAX RCX
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.IntegerMul first second) = do
  loadValue first RAX
  loadValue second RCX
  lift $ emitMUL RAX RCX
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.IntegerDiv first second) = do
  lift $ emitMOV RDX (Number 0)
  loadValue first RAX
  loadValue second RCX
  lift $ emitIDIV RCX
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.IntegerMod first second) = do
  lift $ emitMOV RDX (Number 0)
  loadValue first RAX
  loadValue second RCX
  lift $ emitIDIV RCX
  saveValue tmpRegister RDX
generateQuadrupleOperation tmpRegister (Q.BoolAnd first second) = do
  loadValue first RAX
  loadValue second RCX
  lift $ emitAND RAX RCX
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.BoolOr first second) = do
  loadValue first RAX
  loadValue second RCX
  lift $ emitOR RAX RCX
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.BoolNot location) = do
  loadValue location RCX
  lift $ emitMOV RAX (Number 1)
  lift $ emitSUB RAX RCX
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.StringConcat first second) = do
  let functionCall = (Q.CallFunction "__stringConcat" String [first, second])
  generateQuadrupleOperation tmpRegister functionCall
generateQuadrupleOperation tmpRegister (Q.IntegerCompare first op second) = do
  loadValue first RAX
  loadValue second RCX
  lift $ emitCMP RAX RCX
  generateCompare tmpRegister op
generateQuadrupleOperation tmpRegister (Q.BoolCompare first op second) = do
  loadValue first RAX
  loadValue second RCX
  lift $ emitCMP RAX RCX
  generateCompare tmpRegister op
generateQuadrupleOperation tmpRegister (Q.StringCompare first op second) = do
  let functionCall = (Q.CallFunction "__stringCompare" Int [first, second])
  generateQuadrupleOperation tmpRegister functionCall
  lift $ emitCMP RAX (Number 0)
  generateCompare tmpRegister op
generateQuadrupleOperation tmpRegister (Q.ReturnValue location) = do
  functionEndLabel <- getFunctionEndLabel
  loadValue location RAX
  lift $ emitJMP functionEndLabel
generateQuadrupleOperation tmpRegister Q.ReturnVoid = do
  functionEndLabel <- getFunctionEndLabel
  lift $ emitJMP functionEndLabel
generateQuadrupleOperation tmpRegister (Q.CallFunction ident _ locations) = do
  let registerArguments = take 6 locations
  let stackArguments = drop 6 locations
  mapM_ (uncurry saveArgument) $ zip [0..] registerArguments
  elseLabel <- getNewLabel
  endLabel <- getNewLabel
  lift $ emitTEST RSP (Number 8)
  lift $ emitJXX EQU elseLabel
  lift $ emitSUB RSP (Number 8)
  mapM_ (uncurry saveArgument) $ zip [6..] $ reverse stackArguments
  lift $ emitCALL ident
  lift $ emitADD RSP (Number 8)
  lift $ emitJMP endLabel
  lift $ emitLabel elseLabel
  mapM_ (uncurry saveArgument) $ zip [6..] $ reverse stackArguments
  lift $ emitCALL ident
  lift $ emitLabel endLabel
  when (not $ null stackArguments) $ lift $ emitADD RSP (Number $ (length stackArguments * 8))
  saveValue tmpRegister RAX

generateQuadruple :: Q.BlockNumber -> Q.Quadruple -> C.FunctionGenerator ()
generateQuadruple _ (Q.QuadrupleOperation register operation) = generateQuadrupleOperation register operation
generateQuadruple currentBlockNumber (Q.Jump blockNumber) = do
  blockLabel <- getBlockLabel blockNumber
  saveBlockNumber currentBlockNumber
  lift $ emitJMP blockLabel
generateQuadruple currentBlockNumber (Q.ConditionalJump register first second) = do
  firstLabel <- getBlockLabel first
  secondLabel <- getBlockLabel second
  loadValue register RAX
  saveBlockNumber currentBlockNumber
  lift $ emitCMP RAX (Number 0)
  lift $ emitJXX EQU secondLabel
  lift $ emitJMP firstLabel 

generateBlock :: Q.Block -> C.FunctionGenerator ()
generateBlock block = do
  let blockNumber = view Q.blockNumber block
  let quadruples = view Q.code block
  emitBlockLabel blockNumber
  mapM_ (generateQuadruple blockNumber) quadruples
  saveBlockNumber blockNumber

generateFunction :: Q.FunctionDefinition -> C.FunctionGenerator ()
generateFunction functionDefinition = do
  frameSize <- view C.frameSize <$> get
  emitFunctionLabel
  lift $ emitPUSH RBP
  lift $ emitMOV RBP RSP
  lift $ emitSUB RSP (Number $ frameSize * 8) 
  let blocks = Map.elems $ view Q.blocks functionDefinition
  mapM_ generateBlock blocks
  emitFunctionEndLabel
  lift $ emitADD RSP (Number $ frameSize * 8)
  lift $ emitPOP RBP
  lift $ emitRET

generateHeader :: C.GlobalGenerator ()
generateHeader = do
  emitInstruction "BITS" [64]
  emitGlobal "main"
  emitExtern "printInt"
  emitExtern "printString"
  emitExtern "error"
  emitExtern "readInt"
  emitExtern "readString"
  emitExtern "__stringConcat"
  emitExtern "__stringCompare"
  emitSection Text

generateStrings :: C.GlobalGenerator ()
generateStrings = do
  strings <- Map.keys . view C.strings <$> get
  labels <- mapM getStringLabel strings
  let tuples = zip labels strings
  mapM_ (\(l, s) -> emitConst l "db" ["\"" ++ s ++ "\"",  "0"]) tuples
  
generateBottom :: C.GlobalGenerator ()
generateBottom = do
  emitSection Data
  emitEmptyLine
  generateStrings

generateQuadruples :: Q.Quadruples -> C.GlobalGenerator ()
generateQuadruples quadruples = do
  generateHeader
  emitEmptyLine
  mapM_ runFunctionGenerator $ Map.elems $ view Q.functions quadruples
  emitEmptyLine
  generateBottom

generateMachineCode :: Q.Quadruples -> Either LatteError [String]
generateMachineCode = runGlobalGenerator
-- 
-- Runners
-- 
runGlobalGenerator :: Q.Quadruples -> Either LatteError [String]
runGlobalGenerator quadruples = let
    initialState = C.emptyGlobalContext
  in
    fmap (reverse . view C.code . snd) $ runStateT (generateQuadruples quadruples) initialState

runFunctionGenerator :: Q.FunctionDefinition -> C.GlobalGenerator ()
runFunctionGenerator functionDefinition = let
    initialState = C.emptyFunctionContext functionDefinition
  in
    fmap fst $ runStateT (generateFunction functionDefinition) initialState