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

generateQuadrupleOperation :: Q.TemporaryRegister -> Q.QuadrupleOperation -> C.FunctionGenerator ()
generateQuadrupleOperation tmpRegister (Q.ArgumentInit index) = do
  loadArgument index
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.Assigment location) = do
  loadValue location RAX
  saveValue tmpRegister RAX
generateQuadrupleOperation tmpRegister (Q.PointerStore pointer index value) = do
  loadValue pointer RAX
  loadValue index RCX
  loadValue value RDX
  lift $ emitMOV (PointerOffset RAX RCX 8) RDX
generateQuadrupleOperation tmpRegister (Q.PointerGet pointer index) = do
  loadValue pointer RAX
  loadValue index RCX
  lift $ emitMOV RDX (PointerOffset RAX RCX 8)
  saveValue tmpRegister RDX
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
  let functionCall = (Q.CallFunction "__stringConcat" [first, second])
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
  let functionCall = (Q.CallFunction "__stringCompare" [first, second])
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
generateQuadrupleOperation tmpRegister (Q.CallFunction ident locations) = do
  let registerArguments = take 6 locations
  let stackArguments = drop 6 locations
  stackCounter <- view C.stackCounter <$> lift get
  let stackSize = stackCounter + length stackArguments
  mapM_ (uncurry saveArgument) $ zip [0..] registerArguments
  when (mod stackSize 16 == 0) $ lift $ emitSUB RSP (Number 8)
  mapM_ (uncurry saveArgument) $ zip [6..] $ reverse stackArguments
  lift $ emitCALL ident
  when (not $ null stackArguments) $ lift $ emitADD RSP (Number $ (length stackArguments * 8))
  when (mod stackSize 16 == 0) $ lift $ emitADD RSP (Number 8)
  saveValue tmpRegister RAX


generateQuadruple :: Q.BlockNumber -> Q.Quadruple -> C.FunctionGenerator ()
generateQuadruple _ (Q.QuadrupleOperation register operation) = generateQuadrupleOperation register operation

generatePhi :: Q.Block -> Q.Block -> C.FunctionGenerator ()
generatePhi currentBlock nextBlock = do
  let finalVariables = view Q.finalVariables currentBlock
  let phiVariables = view Q.phiVariables nextBlock
  mapM_ (\(i, r) -> do
      let finalLocation = fromJust $ Map.lookup i finalVariables 
      loadValue finalLocation RAX
      saveValue r RAX
    ) $ Map.toList phiVariables

generateBlockEnding :: Q.Block -> Q.FinalOperation -> C.FunctionGenerator ()
generateBlockEnding block Q.None = return ()
-- TODO
generateBlockEnding block Q.Return = return ()
generateBlockEnding block Q.Jump = do
  nextBlocks <- mapM getBlock $ view Q.nextBlocks block
  let (nextBlock:_) = nextBlocks
  label <- getBlockLabel $ view Q.blockNumber nextBlock
  generatePhi block nextBlock 
  lift $ emitJMP label
generateBlockEnding block (Q.ConditionalJump location) = do
  nextBlocks <- mapM getBlock $ view Q.nextBlocks block
  let (elseBlock:ifBlock:_) = nextBlocks
  ifLabel <- getBlockLabel $ view Q.blockNumber ifBlock 
  elseLabel <- getBlockLabel $ view Q.blockNumber elseBlock
  generatePhi block ifBlock 
  generatePhi block elseBlock
  loadValue location RAX
  lift $ emitCMP RAX (Number 0)
  lift $ emitJXX EQU elseLabel
  lift $ emitJMP ifLabel

generateBlock :: Q.Block -> C.FunctionGenerator ()
generateBlock block = do
  let blockNumber = view Q.blockNumber block
  let quadruples = view Q.code block
  let finalOperation = view Q.finalOperation block
  emitBlockLabel blockNumber
  mapM_ (generateQuadruple blockNumber) quadruples
  generateBlockEnding block finalOperation 

generateFunction :: Q.FunctionDefinition -> C.FunctionGenerator ()
generateFunction functionDefinition = do
  frameSize <- view C.frameSize <$> get
  let frameAddjustment = mod frameSize 2
  emitFunctionLabel
  lift $ emitPUSH RBP
  lift $ emitMOV RBP RSP
  lift $ emitSUB RSP (Number $ (frameSize + frameAddjustment) * 8) 
  let blocks = Map.elems $ view Q.blocks functionDefinition
  mapM_ generateBlock blocks
  emitFunctionEndLabel
  lift $ emitADD RSP (Number $ (frameSize + frameAddjustment) * 8)
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
  emitExtern "debugGcsePrintInt"
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

generateMachineCode :: Q.Quadruples -> Either CompilerError String
generateMachineCode quadruples = case runGlobalGenerator quadruples of
  Right code -> Right code
  Left error -> Left $ CompilerError error NoPosition
-- 
-- Runners
-- 
runGlobalGenerator :: Q.Quadruples -> Either LatteError String
runGlobalGenerator quadruples = let
    initialState = C.emptyGlobalContext
  in
    fmap (unlines . reverse . view C.code . snd) $ runStateT (generateQuadruples quadruples) initialState

runFunctionGenerator :: Q.FunctionDefinition -> C.GlobalGenerator ()
runFunctionGenerator functionDefinition = let
    initialState = C.emptyFunctionContext functionDefinition
  in
    fmap fst $ runStateT (generateFunction functionDefinition) initialState