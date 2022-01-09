{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
module IntermediateCode.Definitions.Quadruples where

import Lens.Micro.Platform
import qualified Data.Map as Map
import Types
import Data.List

-- 
-- Definitions
-- 
type BlockNumber = Int

data TemporaryRegister 
  = TemporaryRegister Type Index
  deriving (Eq, Ord)

data QuadrupleLocation
  = Register TemporaryRegister
  | ConstInt Int 
  | ConstBool Bool
  | ConstString String
  deriving (Eq, Ord)

data QuadrupleOperation
  = ArgumentInit Int Type
  | Phi [(QuadrupleLocation, BlockNumber)]
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
  | CallFunction Ident Type [QuadrupleLocation]
  deriving (Eq, Ord)

data Quadruple 
  = QuadrupleOperation TemporaryRegister QuadrupleOperation
  | Jump BlockNumber
  | ConditionalJump QuadrupleLocation BlockNumber BlockNumber
  | Label Ident BlockNumber
  deriving (Eq, Ord)

data Block 
  = Block {
    _blockNumber :: Int,
    _finalVariables :: Map.Map Ident QuadrupleLocation,
    _previousBlocks :: [BlockNumber],
    _nextBlocks :: [BlockNumber],
    _isAlive :: Bool,
    _code :: [Quadruple],
    _hasReturn :: Bool
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
instance HasType QuadrupleLocation where
  getType (QuadrupleRegister _type _) = _type
  getType (ConstInt _) = Int
  getType (ConstBool _) = Bool
  getType (ConstString _) = String

instance HasType QuadrupleOperation where
  getType (ArgumentInit _ _type) = _type
  getType (IntegerAdd _ _) = Int
  getType (IntegerSub _ _) = Int
  getType (IntegerMul _ _) = Int
  getType (IntegerDiv _ _) = Int
  getType (IntegerMod _ _) = Int
  getType (BoolAnd _ _) = Bool
  getType (BoolOr _ _) = Bool
  getType (BoolNot _) = Bool
  getType (StringConcat _ _) = String
  getType (IntegerCompare _ _ _) = Bool
  getType (BoolCompare _ _ _) = Bool
  getType (StringCompare _ _ _) = Bool
  getType (ReturnValue argument) = getType argument
  getType (ReturnVoid) = Void
  getType (CallFunction _ _type _) = _type

instance Show TemporaryRegister where
  show (TemporaryRegister _ id) = "%t" ++ show id

instance Show QuadrupleLocation where
  show (Register register) = show register
  show (ConstInt x) = show x
  show (ConstBool x) = show x
  show (ConstString x) = show x

instance Show QuadrupleOperation where
  show (ArgumentInit ident _) = "argInit " ++ show ident
  show (Phi locations) = "phi " ++ (concat $ intersperse " " $ map (\(l, b) -> show b ++ ":" ++ show l) locations)
  show (IntegerAdd first second) = "iadd " ++ show first ++ ", " ++ show second
  show (IntegerSub first second) = "isub " ++ show first ++ ", " ++ show second
  show (IntegerMul first second) = "imul " ++ show first ++ ", " ++ show second
  show (IntegerDiv first second) = "idiv " ++ show first ++ ", " ++ show second
  show (IntegerMod first second) = "imod " ++ show first ++ ", " ++ show second
  show (BoolAnd first second) = "and " ++ show first ++ ", " ++ show second
  show (BoolOr first second) = "and " ++ show first ++ ", " ++ show second
  show (BoolNot first) = "not " ++ show first
  show (StringConcat first second) = show (CallFunction "concat" String [first, second])
  show (IntegerCompare first op second) = "icmp " ++ show op ++ ", " ++ show first ++ ", " ++ show second
  show (BoolCompare first op second) = "bcmp " ++ show op ++ ", " ++ show first ++ ", " ++ show second
  show (StringCompare first op second) = "scmp " ++ show op ++ ", " ++ show first ++ ", " ++ show second
  show (ReturnValue first) = "ret " ++ show first
  show (ReturnVoid) = "ret"
  show (CallFunction ident _ locations) = "call " ++ ident ++ " " ++ (concat $ intersperse ", " $ map show locations)

instance Show Quadruple where
  show (QuadrupleOperation register operation) = "\t\t" ++ show register ++ " = " ++ show operation
  show (Jump blockNumber) = "\t\tjmp " ++ show blockNumber
  show (ConditionalJump location first second) = "\t\tjz " ++ show location ++ " " ++ show second ++ " else " ++ show first
  show (Label ident bn) = ident ++ "_" ++ show bn ++ ":"
-- 
-- Functions
-- 
emptyQuadruplesCode :: Quadruples
emptyQuadruplesCode = Quadruples Map.empty

emptyFunctionDefinition :: Ident -> Type -> [Argument] -> FunctionDefinition
emptyFunctionDefinition ident retType args = FunctionDefinition ident retType args 0 Map.empty

getCodeBlock :: Ident -> Block -> [String]
getCodeBlock fi block = let
    bn = view blockNumber block
    label = Label fi bn
    c = map show $ view code block
  in
    "":show label:c
  
getCodeFunction :: FunctionDefinition -> [String]
getCodeFunction fd = let
    bs = Map.elems $ view blocks fd
    fi = view functionIdent fd
  in
    concat $ map (getCodeBlock fi) bs

getCode :: Quadruples -> [String]
getCode quadruples = concat $ map getCodeFunction $ Map.elems $ view functions quadruples
-- 
-- Patterns
-- 
pattern QuadrupleRegister :: Type -> Index -> QuadrupleLocation
pattern QuadrupleRegister _type register = Register (TemporaryRegister _type register)
