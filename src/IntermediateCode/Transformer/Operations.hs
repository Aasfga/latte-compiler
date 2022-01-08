module IntermediateCode.Transformer.Operations where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Errors
import Data.Maybe
import Control.Monad
import Types
import qualified IntermediateCode.Definitions.Quadruples as Q 
import qualified IntermediateCode.Transformer.Context as C
import Lens.Micro.Platform
import IntermediateCode.Transformer.Utilities


addQuadrupleEdge :: Q.BlockNumber -> Q.BlockNumber -> C.FunctionTransformer ()
addQuadrupleEdge source destination = do
  modifyBlock source $ over C.nextBlocks (destination:)
  modifyBlock destination $ over C.previousBlocks (source:)

newQuadrupleBlock :: Bool -> C.FunctionTransformer Q.BlockNumber
newQuadrupleBlock isAlive = do
  assertNotInQuadrupleBlock
  newBlockNumber <- getNewBlockNumber
  setCurrentBlockNumber newBlockNumber
  let newBlock = C.emptyBlockContext newBlockNumber isAlive
  modify $ over C.blocks (Map.insert newBlockNumber newBlock)
  scope <- getCurrentScope
  mapM_ addPhiPlaceholder scope
  return newBlockNumber

leaveQuadrupleBlock :: C.FunctionTransformer ()
leaveQuadrupleBlock = do
  scope <- getCurrentScope
  locations <- mapM getLocation scope
  modifyCurrentBlock $ set C.finalVariables (Map.fromList $ zip scope locations)
  modify $ set C.currentBlockNumber Nothing

addQuadrupleOperation :: Q.QuadrupleOperation -> C.FunctionTransformer Q.TemporaryRegister
addQuadrupleOperation operation = do
  newRegisterNumber <- getNewRegisterNumber
  let operationType = getType operation
  let resultRegister = Q.TemporaryRegister operationType newRegisterNumber
  let quadruple = C.Quadruple $ Q.QuadrupleOperation resultRegister operation
  modifyCurrentBlock $ over C.code (quadruple:)
  return resultRegister
-- 
-- Operations
-- 
addPhiPlaceholder :: Ident -> C.FunctionTransformer ()
addPhiPlaceholder ident = do
  let placeholder = C.PhiPlaceholder ident
  modifyCurrentBlock $ over C.code (placeholder:)

addJumpPlaceholder :: C.FunctionTransformer ()
addJumpPlaceholder = do
  modifyCurrentBlock $ over C.code (C.JumpPlaceholder:)

addConditionalJumpPlaceholder :: Q.QuadrupleLocation -> C.FunctionTransformer ()
addConditionalJumpPlaceholder location = do
  modifyCurrentBlock $ over C.code (C.ConditionalJumpPlaceholder location:)

argumentInit :: Index -> Argument -> C.FunctionTransformer ()
argumentInit index (Argument _type ident) = do
  let operation = Q.ArgumentInit index _type
  register <- addQuadrupleOperation operation
  newVariable _type ident $ Q.Register register 

integerAdd :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerAdd (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (x + y)
integerAdd first second = do
  assertLocationType first Int
  assertLocationType second Int
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.IntegerAdd minValue maxValue
  Q.Register <$> addQuadrupleOperation operation

integerSub :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerSub (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (x - y)
integerSub first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerSub first second
  Q.Register <$> addQuadrupleOperation operation

integerMul :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerMul (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (x * y)
integerMul first second = do
  assertLocationType first Int
  assertLocationType second Int
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.IntegerMul minValue maxValue
  Q.Register <$> addQuadrupleOperation operation

integerDiv :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerDiv (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (div x  y)
integerDiv first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerDiv first second
  Q.Register <$> addQuadrupleOperation operation

integerMod :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerMod (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (mod x y)
integerMod first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerMod first second
  Q.Register <$> addQuadrupleOperation operation

boolAnd :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
boolAnd (Q.ConstBool x) (Q.ConstBool y) = return $ Q.ConstBool (x && y)
boolAnd first second = do
  assertLocationType first Bool
  assertLocationType second Bool 
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.BoolAnd minValue maxValue
  Q.Register <$> addQuadrupleOperation operation

boolOr :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
boolOr (Q.ConstBool x) (Q.ConstBool y) = return $ Q.ConstBool (x || y)
boolOr first second = do
  assertLocationType first Bool
  assertLocationType second Bool 
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.BoolOr minValue maxValue
  Q.Register <$> addQuadrupleOperation operation

boolNot :: Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
boolNot (Q.ConstBool x) = return $ Q.ConstBool (not x)
boolNot location = do
  assertLocationType location Bool
  let operation = Q.BoolNot location
  Q.Register <$> addQuadrupleOperation operation

stringConcat :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
stringConcat (Q.ConstString x) (Q.ConstString y) = return $ Q.ConstString (x ++ y)
stringConcat first second = do
  assertLocationType first String
  assertLocationType second String
  let operation = Q.StringConcat first second
  Q.Register <$> addQuadrupleOperation operation 

integerCompare :: Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerCompare (Q.ConstInt x) op (Q.ConstInt y) = return $ Q.ConstBool $ getCompareFunction op x y
integerCompare first op second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerCompare first op second
  Q.Register <$> addQuadrupleOperation operation

stringCompare :: Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
stringCompare (Q.ConstString x) op (Q.ConstString y) = return $ Q.ConstBool $ getCompareFunction op x y
stringCompare first op second = do
  assertLocationType first String
  assertLocationType second String 
  let operation = Q.StringCompare first op second
  Q.Register <$> addQuadrupleOperation operation

boolCompare :: Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
boolCompare (Q.ConstBool x) op (Q.ConstBool y) = return $ Q.ConstBool $ getCompareFunction op x y
boolCompare first op second = do
  assertLocationType first String
  assertLocationType second String 
  let operation = Q.BoolCompare first op second
  Q.Register <$> addQuadrupleOperation operation

returnExpression :: Q.QuadrupleLocation -> C.FunctionTransformer ()
returnExpression location = do
  assertReturnTypeIsCorrect $ getType location
  let operation = Q.ReturnValue location
  void $ addQuadrupleOperation operation

returnVoid :: C.FunctionTransformer ()
returnVoid = do
  assertReturnTypeIsCorrect Void
  let operation = Q.ReturnVoid
  void $ addQuadrupleOperation operation

callFunction :: Ident -> [Q.QuadrupleLocation] -> C.FunctionTransformer Q.QuadrupleLocation
callFunction ident locations = do
  let locationTypes = map getType locations 
  (returnType, argumentTypes) <- lift $ getFunctionType ident
  unless (argumentTypes == locationTypes) $ throwError $ TypeMissmatchApplication ident argumentTypes locationTypes
  let operation = Q.CallFunction ident returnType locations
  resultRegister <- addQuadrupleOperation operation
  return $ Q.Register resultRegister