module IntermediateCode.Transformer.QuadrupleUtilities where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Errors
import Data.Maybe
import Control.Monad
import Types
import qualified IntermediateCode.Definitions.Quadruples as Q 
import qualified IntermediateCode.Transformer.TransformerContext as C
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
  let operationType = Q.getOperationType operation
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

addConditionalJumpPlaceholder :: C.Location -> C.FunctionTransformer ()
addConditionalJumpPlaceholder location = do
  modifyCurrentBlock $ over C.code (C.ConditionalJumpPlaceholder location:)

argumentInit :: Index -> Argument -> C.FunctionTransformer ()
argumentInit index (Argument _type ident) = do
  let operation = Q.ArgumentInit index _type
  register <- addQuadrupleOperation operation
  newVariable _type ident $ Q.Register register 

integerAdd :: C.Location -> C.Location -> C.FunctionTransformer C.Location
integerAdd (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (x + y)
integerAdd first second = do
  assertLocationType first Int
  assertLocationType second Int
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.IntegerAdd minValue maxValue
  Q.Register <$> addQuadrupleOperation operation

integerSub :: C.Location -> C.Location -> C.FunctionTransformer C.Location
integerSub (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (x - y)
integerSub first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerSub first second
  Q.Register <$> addQuadrupleOperation operation

integerMul :: C.Location -> C.Location -> C.FunctionTransformer C.Location
integerMul (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (x * y)
integerMul first second = do
  assertLocationType first Int
  assertLocationType second Int
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.IntegerMul minValue maxValue
  Q.Register <$> addQuadrupleOperation operation

integerDiv :: C.Location -> C.Location -> C.FunctionTransformer C.Location
integerDiv (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (div x  y)
integerDiv first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerDiv first second
  Q.Register <$> addQuadrupleOperation operation

integerMod :: C.Location -> C.Location -> C.FunctionTransformer C.Location
integerMod (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (mod x y)
integerMod first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerMod first second
  Q.Register <$> addQuadrupleOperation operation

boolAnd :: C.Location -> C.Location -> C.FunctionTransformer C.Location
boolAnd (Q.ConstBool x) (Q.ConstBool y) = return $ Q.ConstBool (x && y)
boolAnd first second = do
  assertLocationType first Bool
  assertLocationType second Bool 
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.BoolAnd minValue maxValue
  Q.Register <$> addQuadrupleOperation operation

boolOr :: C.Location -> C.Location -> C.FunctionTransformer C.Location
boolOr (Q.ConstBool x) (Q.ConstBool y) = return $ Q.ConstBool (x || y)
boolOr first second = do
  assertLocationType first Bool
  assertLocationType second Bool 
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.BoolOr minValue maxValue
  Q.Register <$> addQuadrupleOperation operation

boolNot :: C.Location -> C.FunctionTransformer C.Location
boolNot (Q.ConstBool x) = return $ Q.ConstBool (not x)
boolNot location = do
  assertLocationType location Bool
  let operation = Q.BoolNot location
  Q.Register <$> addQuadrupleOperation operation

stringConcat :: C.Location -> C.Location -> C.FunctionTransformer C.Location
stringConcat (Q.ConstString x) (Q.ConstString y) = return $ Q.ConstString (x ++ y)
stringConcat first second = do
  assertLocationType first String
  assertLocationType second String
  let operation = Q.StringConcat first second
  Q.Register <$> addQuadrupleOperation operation 

integerCompare :: C.Location -> CompareOperation -> C.Location -> C.FunctionTransformer C.Location
integerCompare (Q.ConstInt x) op (Q.ConstInt y) = return $ Q.ConstBool $ getCompareFunction op x y
integerCompare first op second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerCompare first op second
  Q.Register <$> addQuadrupleOperation operation

stringCompare :: C.Location -> CompareOperation -> C.Location -> C.FunctionTransformer C.Location
stringCompare (Q.ConstString x) op (Q.ConstString y) = return $ Q.ConstBool $ getCompareFunction op x y
stringCompare first op second = do
  assertLocationType first String
  assertLocationType second String 
  let operation = Q.StringCompare first op second
  Q.Register <$> addQuadrupleOperation operation

boolCompare :: C.Location -> CompareOperation -> C.Location -> C.FunctionTransformer C.Location
boolCompare (Q.ConstBool x) op (Q.ConstBool y) = return $ Q.ConstBool $ getCompareFunction op x y
boolCompare first op second = do
  assertLocationType first String
  assertLocationType second String 
  let operation = Q.BoolCompare first op second
  Q.Register <$> addQuadrupleOperation operation

returnExpression :: C.Location -> C.FunctionTransformer ()
returnExpression location = do
  assertReturnTypeIsCorrect $ C.getLocationType location
  let operation = Q.ReturnValue location
  void $ addQuadrupleOperation operation

returnVoid :: C.FunctionTransformer ()
returnVoid = do
  assertReturnTypeIsCorrect Void
  let operation = Q.ReturnVoid
  void $ addQuadrupleOperation operation

callFunction :: Ident -> [C.Location] -> C.FunctionTransformer C.Location
callFunction ident locations = do
  let locationTypes = map C.getLocationType locations 
  (returnType, argumentTypes) <- lift $ getFunctionType ident
  unless (argumentTypes == locationTypes) $ throwError $ TypeMissmatchApplication ident argumentTypes locationTypes
  let operation = Q.CallFunction ident returnType locations
  resultRegister <- addQuadrupleOperation operation
  return $ Q.Register resultRegister