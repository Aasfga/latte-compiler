{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module IntermediateCode.Definitions.Quadruples where

import Lens.Micro.Platform
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types
import Data.List

-- 
-- Definitions
-- 
type BlockNumber = Int

type TemporaryRegister = Int

data QuadrupleLocation
  = Register TemporaryRegister
  | ConstInt Int 
  | ConstBool Bool
  | ConstString String
  deriving (Eq, Ord)

data FinalOperation
  = Jump
  | ConditionalJump QuadrupleLocation
  | Return
  | None

data QuadrupleOperation
  = ArgumentInit Int
  | Assigment QuadrupleLocation
  | IntegerAdd QuadrupleLocation QuadrupleLocation
  | IntegerSub QuadrupleLocation QuadrupleLocation
  | IntegerMul QuadrupleLocation QuadrupleLocation
  | IntegerDiv QuadrupleLocation QuadrupleLocation
  | IntegerMod QuadrupleLocation QuadrupleLocation
  | BoolAnd QuadrupleLocation QuadrupleLocation
  | BoolOr QuadrupleLocation QuadrupleLocation
  | BoolNot QuadrupleLocation
  | StringConcat QuadrupleLocation QuadrupleLocation
  | IntegerCompare QuadrupleLocation CompareOperation QuadrupleLocation
  | BoolCompare QuadrupleLocation CompareOperation QuadrupleLocation
  | StringCompare QuadrupleLocation CompareOperation QuadrupleLocation
  | ReturnValue QuadrupleLocation
  | ReturnVoid
  | CallFunction Ident [QuadrupleLocation]
  deriving (Eq, Ord)

data Quadruple = QuadrupleOperation TemporaryRegister QuadrupleOperation
  deriving (Eq, Ord)

data Block 
  = Block {
    _blockNumber :: BlockNumber,
    _phiVariables :: Map.Map Ident TemporaryRegister, 
    _requiredRegisters :: Set.Set TemporaryRegister,
    _finalVariables :: Map.Map Ident QuadrupleLocation,
    _previousBlocks :: [BlockNumber],
    _nextBlocks :: [BlockNumber],
    _isAlive :: Bool,
    _code :: [Quadruple],
    _hasReturn :: Bool,
    _finalOperation :: FinalOperation
  }

data FunctionDefinition
  = FunctionDefinition {
    _functionIdent :: Ident, 
    _returnType :: Type,
    _arguments :: [Argument],
    _registerCounter :: Int,
    _blocks :: Map.Map BlockNumber Block
  }

data Quadruples 
  = Quadruples {
    _functions :: Map.Map Ident FunctionDefinition
  }

$(makeLenses ''Block)
$(makeLenses ''FunctionDefinition)
$(makeLenses ''Quadruples)
-- 
-- Instances
-- 
instance Show FinalOperation where
  show None = "None"
  show Jump = "Jump"
  show (ConditionalJump location) = "Conditional jump based on " ++ show location
  show Return = "Return"
 
instance Show QuadrupleLocation where
  show (Register register) = "%t" ++ show register
  show (ConstInt x) = show x
  show (ConstBool x) = show x
  show (ConstString x) = show x

instance Show QuadrupleOperation where
  show (ArgumentInit ident) = "argInit " ++ show ident
  show (Assigment l) = show l
  show (IntegerAdd first second) = "iadd " ++ show first ++ ", " ++ show second
  show (IntegerSub first second) = "isub " ++ show first ++ ", " ++ show second
  show (IntegerMul first second) = "imul " ++ show first ++ ", " ++ show second
  show (IntegerDiv first second) = "idiv " ++ show first ++ ", " ++ show second
  show (IntegerMod first second) = "imod " ++ show first ++ ", " ++ show second
  show (BoolAnd first second) = "and " ++ show first ++ ", " ++ show second
  show (BoolOr first second) = "and " ++ show first ++ ", " ++ show second
  show (BoolNot first) = "not " ++ show first
  show (StringConcat first second) = show (CallFunction "concat" [first, second])
  show (IntegerCompare first op second) = "icmp " ++ show op ++ ", " ++ show first ++ ", " ++ show second
  show (BoolCompare first op second) = "bcmp " ++ show op ++ ", " ++ show first ++ ", " ++ show second
  show (StringCompare first op second) = "scmp " ++ show op ++ ", " ++ show first ++ ", " ++ show second
  show (ReturnValue first) = "ret " ++ show first
  show (ReturnVoid) = "ret"
  show (CallFunction ident locations) = "call " ++ ident ++ " " ++ (concat $ intersperse ", " $ map show locations)

instance Show Quadruple where
  show (QuadrupleOperation register operation) = "\t\t%t" ++ show register ++ " = " ++ show operation

instance Show Block where
  show __block = let
      __code = unlines . map show $ view code __block
      phis = unlines . map (\(i, l) -> "\t\t%t" ++ show l ++ " = phi " ++ i) $ Map.toList $ view phiVariables __block
    in
      phis ++ __code

instance Show FunctionDefinition where
  show functionDefinition = let
      __functionIdent = view functionIdent functionDefinition
      __blocks = Map.elems $ view blocks functionDefinition
      blockNumbers = map (view blockNumber) __blocks
      blocksLabels = map (\bn -> __functionIdent ++ show bn ++ ":") blockNumbers
      blocksCode = map show __blocks
    in
      unlines $ map (\(l, c) -> l ++ "\n" ++ c) $ zip blocksLabels blocksCode

instance Show Quadruples where
  show quadruples = let
      __functions = Map.elems $ view functions quadruples
      labels = map (\f -> (view functionIdent f) ++ ":") __functions
      functionsCode = map show __functions 
    in
      unlines $ map (\(l, c) -> l ++ "\n" ++ c) $ zip labels functionsCode

instance DebugShow Block where
  debugShow block = let
      __phiVariables = Map.toList $ view phiVariables block
      __requiredRegisters = Set.toList $ view requiredRegisters block
      __finalVariables = Map.toList $ view finalVariables block
      __previousBlocks = view previousBlocks block
      __nextBlocks = view nextBlocks block
      __isAlive = view isAlive block
      __code = unlines . map show $ view code block
      __hasReturn = view hasReturn block
      __finalOperation = view finalOperation block
      phis = unlines . map (\(i, l) -> "\t\t%t" ++ show l ++ " = phi " ++ i) $ Map.toList $ view phiVariables block
    in
      "phiVariables: " ++ show __phiVariables ++ "\n" ++
      "requiredRegisters:" ++ show __requiredRegisters ++ "\n" ++ 
      "finalVariables: " ++ show __finalVariables ++ "\n" ++ 
      "previousBlocks: " ++ show __previousBlocks ++ "\n" ++ 
      "nextBlocks: " ++ show __nextBlocks ++ "\n" ++ 
      "isAlive: " ++ show __isAlive ++ "\n" ++
      "hasReturn: " ++ show __hasReturn ++ "\n" ++
      "finalOperation: " ++ show __finalOperation ++ "\n" ++ 
      phis ++ __code

instance DebugShow FunctionDefinition where
  debugShow functionDefinition = let
      __functionIdent = view functionIdent functionDefinition
      __blocks = Map.elems $ view blocks functionDefinition
      blockNumbers = map (view blockNumber) __blocks
      blocksLabels = map (\bn -> __functionIdent ++ show bn ++ ":") blockNumbers
      blocksCode = map debugShow __blocks
    in
      unlines $ map (\(l, c) -> l ++ "\n" ++ c) $ zip blocksLabels blocksCode


instance DebugShow Quadruples where 
  debugShow quadruples = let
      __functions = Map.elems $ view functions quadruples
      labels = map (\f -> "#####" ++ (view functionIdent f)) __functions
      functionsCode = map debugShow __functions 
    in
      unlines $ map (\(l, c) -> l ++ "\n" ++ c) $ zip labels functionsCode

-- 
-- Functions
-- 
emptyQuadruplesCode :: Quadruples
emptyQuadruplesCode = Quadruples Map.empty

emptyFunctionDefinition :: Ident -> Type -> [Argument] -> FunctionDefinition
emptyFunctionDefinition ident retType args = FunctionDefinition ident retType args 0 Map.empty

getRegister :: QuadrupleLocation -> Maybe TemporaryRegister
getRegister (Register register) = Just register
getRegister _ = Nothing

getRegisters :: QuadrupleOperation -> [TemporaryRegister]
getRegisters (ArgumentInit _) = []
getRegisters (Assigment l) = catMaybes $ map getRegister [l]
getRegisters (IntegerAdd first second) = catMaybes $ map getRegister [first, second] 
getRegisters (IntegerSub first second) = catMaybes $ map getRegister [first, second]
getRegisters (IntegerMul first second) = catMaybes $ map getRegister [first, second]
getRegisters (IntegerDiv first second) = catMaybes $ map getRegister [first, second]
getRegisters (IntegerMod first second) = catMaybes $ map getRegister [first, second]
getRegisters (BoolAnd first second) = catMaybes $ map getRegister [first, second]
getRegisters (BoolOr first second) = catMaybes $ map getRegister [first, second]
getRegisters (BoolNot first) = catMaybes $ map getRegister [first]
getRegisters (StringConcat first second) = catMaybes $ map getRegister [first, second]
getRegisters (IntegerCompare first op second) = catMaybes $ map getRegister [first, second]
getRegisters (BoolCompare first op second) = catMaybes $ map getRegister [first, second]
getRegisters (StringCompare first op second) = catMaybes $ map getRegister [first, second]
getRegisters (ReturnValue first) = catMaybes $ map getRegister [first]
getRegisters (ReturnVoid) = []
getRegisters (CallFunction ident locations) = catMaybes $ map getRegister locations