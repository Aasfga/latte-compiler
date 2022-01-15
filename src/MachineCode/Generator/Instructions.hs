module MachineCode.Generator.Instructions where

import Control.Monad.Except
import Errors
import Control.Monad.State
import MachineCode.Types
import MachineCode.Generator.Utilities
import qualified IntermediateCode.Definitions.Quadruples as Q 
import qualified MachineCode.Generator.Context as C
import Types
import Lens.Micro.Platform



loadValue :: Q.QuadrupleLocation -> ValueLocation -> C.FunctionGenerator ()
loadValue (Q.Register index) register = do
  lift $ emitMOV register (FrameOffset (-index * 8))
loadValue (Q.ConstInt int) register = do
  lift $ emitMOV register (Number int)
loadValue (Q.ConstBool False) register = do
  lift $ emitMOV register (Number 0)
loadValue (Q.ConstBool True) register = do
  lift $ emitMOV register (Number 1)
loadValue (Q.ConstString string) register = do
  stringLabel <- lift (getStringLabel string)
  lift $ emitMOV register (LabeledMemory stringLabel)

saveValue :: C.ValueIndex -> ValueLocation -> C.FunctionGenerator ()
saveValue index register = do 
  lift $ emitMOV (FrameOffset (-index * 8)) register

saveArgument :: Index -> Q.QuadrupleLocation -> C.FunctionGenerator ()
saveArgument index location
  | index >= 0 && index < 6 = loadValue location $ getArgumentLocation index
  | index >= 6 = do
    loadValue location RAX
    lift $ emitPUSH RAX
  | otherwise = error "Index must be greater than 0"
  
loadArgument :: Index -> C.FunctionGenerator ()
loadArgument index = lift $ emitMOV RAX $ getArgumentLocation index

saveBlockNumber :: Q.BlockNumber -> C.FunctionGenerator ()
saveBlockNumber blockNumber = lift $ emitMOV R8 (Number blockNumber)

emitUnaryOperation :: String -> ValueLocation -> C.GlobalGenerator ()
emitUnaryOperation mnemonic location = emitInstruction mnemonic [location]

emitBinaryInstruction :: String -> ValueLocation -> ValueLocation -> C.GlobalGenerator ()
emitBinaryInstruction mnemonic first second = emitInstruction mnemonic [first, second]

emitMOV :: ValueLocation -> ValueLocation -> C.GlobalGenerator ()
emitMOV = emitBinaryInstruction "mov"

emitADD :: ValueLocation -> ValueLocation -> C.GlobalGenerator ()
emitADD RSP (Number x) = do
  modify $ over C.stackCounter (x-)
  emitBinaryInstruction "add" RSP (Number x)
emitADD first second = emitBinaryInstruction "add" first second

emitSUB :: ValueLocation -> ValueLocation -> C.GlobalGenerator ()
emitSUB RSP (Number x) = do
  modify $ over C.stackCounter (x+)
  emitBinaryInstruction "sub" RSP (Number x)
emitSUB first second = emitBinaryInstruction "sub" first second

emitMUL :: ValueLocation -> ValueLocation -> C.GlobalGenerator ()
emitMUL = emitBinaryInstruction "imul"

emitIDIV :: ValueLocation -> C.GlobalGenerator ()
emitIDIV = emitUnaryOperation "idiv"

emitAND :: ValueLocation -> ValueLocation -> C.GlobalGenerator ()
emitAND = emitBinaryInstruction "and"

emitOR :: ValueLocation -> ValueLocation -> C.GlobalGenerator ()
emitOR = emitBinaryInstruction "or"

emitNOT :: ValueLocation -> C.GlobalGenerator ()
emitNOT = emitUnaryOperation "not"

emitJMP :: Label -> C.GlobalGenerator ()
emitJMP label = emitInstruction "jmp" [LabeledMemory label]

emitJXX :: CompareOperation -> Label -> C.GlobalGenerator ()
emitJXX op label = do
  mnemonic <- return $ case op of
    LTH -> "jl"
    LE -> "jle"
    GTH -> "jg"
    GE -> "jge"
    EQU -> "je"
    NE -> "jne"
  emitInstruction mnemonic [LabeledMemory label]

emitPUSH :: ValueLocation -> C.GlobalGenerator ()
emitPUSH location = do
  modify $ over C.stackCounter (8+)
  emitInstruction "push" [location]

emitPOP :: ValueLocation -> C.GlobalGenerator ()
emitPOP location = do
  modify $ over C.stackCounter (8-)
  emitInstruction "pop" [location]

emitRET :: C.GlobalGenerator ()
emitRET = emitInstruction "ret" ([] :: [String])

emitTEST :: ValueLocation -> ValueLocation -> C.GlobalGenerator ()
emitTEST = emitBinaryInstruction "test"

emitCMP :: ValueLocation -> ValueLocation -> C.GlobalGenerator ()
emitCMP = emitBinaryInstruction "cmp"

emitCALL :: Label -> C.GlobalGenerator ()
emitCALL label = emitInstruction "call" [LabeledMemory label]

emitSYSCALL :: C.GlobalGenerator ()
emitSYSCALL = emitInstruction "syscall" ([] :: [String])