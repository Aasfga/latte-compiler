module IntermediateCode.Transformer.Operations where

import qualified Data.Map as Map
import Control.Monad.State
import Errors
import Data.Maybe
import Control.Monad
import Types
import qualified IntermediateCode.Definitions.Quadruples as Q 
import qualified IntermediateCode.Transformer.Context as C
import Lens.Micro.Platform
import IntermediateCode.Transformer.Utilities
import Debug.Trace


addQuadrupleEdge :: Q.BlockNumber -> Q.BlockNumber -> C.FunctionTransformer ()
addQuadrupleEdge source destination = do
  modifyBlock source $ over C.nextBlocks (destination:)
  modifyBlock destination $ over C.previousBlocks (source:)

removeQuadrupleEdge :: Q.BlockNumber -> Q.BlockNumber -> C.FunctionTransformer ()
removeQuadrupleEdge source destination = do
  modifyBlock source $ over C.nextBlocks (filter (destination /=))
  modifyBlock destination $ over C.previousBlocks (filter (source /=))
  
newQuadrupleBlock :: Bool -> C.FunctionTransformer Q.BlockNumber
newQuadrupleBlock isAlive = do
  assertNotInQuadrupleBlock
  newBlockNumber <- getNewBlockNumber
  setCurrentBlockNumber newBlockNumber
  idents <- Map.keys . Map.filter (not . null) . view C.variables <$> get
  types <- mapM (\ident -> getLocation ident >>= getLocationType) idents
  newRegisters <- mapM getNewRegisterNumber types
  let newLocations = map Q.Register newRegisters
  mapM_ (uncurry setLocation) $ zip idents newLocations
  let newBlock = C.emptyBlockContext newBlockNumber isAlive (Map.fromList $ zip idents newRegisters)
  modify $ over C.blocks (Map.insert newBlockNumber newBlock)
  return newBlockNumber

gentlyLeaveQuadrupleBlock :: Q.FinalOperation -> C.FunctionTransformer ()
gentlyLeaveQuadrupleBlock finalOperation = do
  currentBlockNumber <- view C.currentBlockNumber <$> get
  case currentBlockNumber of
    Nothing -> return ()
    _ -> leaveQuadrupleBlock finalOperation

leaveQuadrupleBlock :: Q.FinalOperation -> C.FunctionTransformer ()
leaveQuadrupleBlock finalOperation = do
  variables <- Map.toList . view C.variables <$> get
  let finalVariables = map (\(ident, x:_) -> (ident, x)) (filter (not . null . snd) variables)
  modifyCurrentBlock $ set C.finalVariables $ Map.fromList finalVariables
  modifyCurrentBlock $ set C.finalOperation finalOperation
  modify $ set C.currentBlockNumber Nothing

addQuadrupleOperation :: Q.QuadrupleOperation -> Type -> C.FunctionTransformer Q.QuadrupleLocation
addQuadrupleOperation operation operationType = do
  register <- getNewRegisterNumber operationType
  let quadruple = C.Quadruple $ Q.QuadrupleOperation register operation
  modifyCurrentBlock $ over C.code (quadruple:)
  return $ Q.Register register
-- 
-- Operations
-- 
argumentInit :: Index -> Argument -> C.FunctionTransformer ()
argumentInit index (Argument _type ident) = do
  let operation = Q.ArgumentInit index
  location <- addQuadrupleOperation operation _type
  newVariable _type ident location

cast :: Type -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
cast _type location = do
  let operation = Q.Assigment location
  addQuadrupleOperation operation _type

createObject :: Type -> C.FunctionTransformer Q.QuadrupleLocation
createObject (Object classIdent) = do
  lift $ assertClassExists classIdent
  let constructorIdent = getConstructorIdent classIdent
  callFunction constructorIdent []
createObject _type = throwErrorFunction $ NotAClass _type

objectStore :: Q.QuadrupleLocation -> Ident -> Q.QuadrupleLocation -> C.FunctionTransformer ()
objectStore object memberIdent value = do
  valueType <- getLocationType value
  memberType <- getMemberType object memberIdent
  unless (memberType == valueType) $ throwErrorFunction $ TypeMissmatch memberType valueType
  memberIndex <- getMemberIndex object memberIdent
  pointerStore object (Q.ConstInt memberIndex) value

objectGet :: Q.QuadrupleLocation -> Ident -> C.FunctionTransformer Q.QuadrupleLocation
objectGet object memberIdent = do
  memberIndex <- getMemberIndex object memberIdent
  memberType <- getMemberType object memberIdent
  pointerGet object (Q.ConstInt memberIndex) memberType

createArray :: Type -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
createArray _type index = do
  assertLocationType index Int
  let operation = Q.CallFunction "__createArray" [index]
  addQuadrupleOperation operation $ Array _type

arrayLength :: Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
arrayLength array = do
  assertIsArray array
  pointerGet array (Q.ConstInt 1) Int

arrayGet :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
arrayGet array index = do
  valueType <- getArrayValueType array
  assertLocationType index Int
  actualArrayLocation <- pointerGet array (Q.ConstInt 0) Int
  pointerGet actualArrayLocation index valueType

arrayStore :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer ()
arrayStore array index value = do
  valueType <- getLocationType value
  assertArrayValueType array valueType
  actualArrayLocation <- pointerGet array (Q.ConstInt 0) Int
  pointerStore actualArrayLocation index value

pointerStore :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer ()
pointerStore pointer index value = do
  assertLocationType index Int
  let operation = Q.PointerStore pointer index value
  void $ addQuadrupleOperation operation Void

pointerGet :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> Type -> C.FunctionTransformer Q.QuadrupleLocation
pointerGet pointer index _type = do
  let operation = Q.PointerGet pointer index
  addQuadrupleOperation operation _type

integerAdd :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerAdd (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (x + y)
integerAdd first second = do
  assertLocationType first Int
  assertLocationType second Int
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.IntegerAdd minValue maxValue
  addQuadrupleOperation operation Int

integerSub :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerSub (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (x - y)
integerSub first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerSub first second
  addQuadrupleOperation operation Int

integerMul :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerMul (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (x * y)
integerMul first second = do
  assertLocationType first Int
  assertLocationType second Int
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.IntegerMul minValue maxValue
  addQuadrupleOperation operation Int

integerDiv :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerDiv (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (div x  y)
integerDiv first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerDiv first second
  addQuadrupleOperation operation Int

integerMod :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerMod (Q.ConstInt x) (Q.ConstInt y) = return $ Q.ConstInt (mod x y)
integerMod first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerMod first second
  addQuadrupleOperation operation Int

boolAnd :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
boolAnd (Q.ConstBool x) (Q.ConstBool y) = return $ Q.ConstBool (x && y)
boolAnd first second = do
  assertLocationType first Bool
  assertLocationType second Bool 
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.BoolAnd minValue maxValue
  addQuadrupleOperation operation Int

boolOr :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
boolOr (Q.ConstBool x) (Q.ConstBool y) = return $ Q.ConstBool (x || y)
boolOr first second = do
  assertLocationType first Bool
  assertLocationType second Bool 
  let minValue = min first second
  let maxValue = max first second
  let operation = Q.BoolOr minValue maxValue
  addQuadrupleOperation operation Bool

boolNot :: Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
boolNot (Q.ConstBool x) = return $ Q.ConstBool (not x)
boolNot location = do
  assertLocationType location Bool
  let operation = Q.BoolNot location
  addQuadrupleOperation operation Bool

stringConcat :: Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
stringConcat (Q.ConstString x) (Q.ConstString y) = return $ Q.ConstString (x ++ y)
stringConcat first second = do
  assertLocationType first String
  assertLocationType second String
  let operation = Q.StringConcat first second
  addQuadrupleOperation operation String

integerCompare :: Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
integerCompare (Q.ConstInt x) op (Q.ConstInt y) = return $ Q.ConstBool $ getCompareFunction op x y
integerCompare first op second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = Q.IntegerCompare first op second
  addQuadrupleOperation operation Bool

stringCompare :: Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
stringCompare (Q.ConstString x) op (Q.ConstString y) = return $ Q.ConstBool $ getCompareFunction op x y
stringCompare first op second = do
  assertLocationType first String
  assertLocationType second String 
  let operation = Q.StringCompare first op second
  addQuadrupleOperation operation Bool

objectCompare :: Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
objectCompare first op second = do
  assertIsObject first
  assertIsObject second
  pointerCompare first op second

arrayCompare :: Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
arrayCompare first op second = do
  assertIsArray first 
  assertIsArray second
  pointerCompare first op second

pointerCompare :: Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
pointerCompare first op second = do
  let operation = Q.CallFunction "__pointerCompare" [first, second]
  result <- addQuadrupleOperation operation Int
  integerCompare result op $ Q.ConstInt 0

boolCompare :: Q.QuadrupleLocation -> CompareOperation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
boolCompare (Q.ConstBool x) op (Q.ConstBool y) = return $ Q.ConstBool $ getCompareFunction op x y
boolCompare first op second = do
  assertLocationType first Bool
  assertLocationType second Bool
  let operation = Q.BoolCompare first op second
  addQuadrupleOperation operation Bool

returnExpression :: Q.QuadrupleLocation -> C.FunctionTransformer ()
returnExpression location = do
  locationType <- getLocationType location
  assertReturnTypeIsCorrect locationType
  let operation = Q.ReturnValue location
  void $ addQuadrupleOperation operation locationType

returnVoid :: C.FunctionTransformer ()
returnVoid = do
  assertReturnTypeIsCorrect Void
  let operation = Q.ReturnVoid
  void $ addQuadrupleOperation operation Void

callFunction :: Ident -> [Q.QuadrupleLocation] -> C.FunctionTransformer Q.QuadrupleLocation
callFunction ident locations = do
  locationTypes <- mapM getLocationType locations 
  (returnType, argumentTypes) <- lift $ getFunctionType ident
  unless (argumentTypes == locationTypes) $ throwErrorFunction $ TypeMissmatchApplication ident argumentTypes locationTypes
  let operation = Q.CallFunction ident locations
  addQuadrupleOperation operation returnType