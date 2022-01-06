
{-# LANGUAGE PatternSynonyms #-}
module IntermediateCode.Transformer where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Errors
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Types
import IntermediateCode.Definitions.AbstractSyntaxTree as AST
import IntermediateCode.Definitions.Quadruples as Q 
import IntermediateCode.TransformerContext as C
import Lens.Micro.Platform

-- 
-- Transformer
-- 
transformBinaryOperation :: Type -> Operation -> Location -> Location -> FunctionTransformer Location
transformBinaryOperation Int Plus = integerAdd
transformBinaryOperation Int Minus = integerSub
transformBinaryOperation Int Times = integerMul
transformBinaryOperation Int Div = integerDiv
transformBinaryOperation Int Mod = integerMod
transformBinaryOperation Bool And = boolAnd
transformBinaryOperation Bool Or = boolOr
transformBinaryOperation String Plus = stringConcat
transformBinaryOperation _type op = (\_ _ -> throwError $ TypeMissmatchBinaryOperator _type _type op)

transformExpression :: Expression -> FunctionTransformer Location 
transformExpression (Variable p ident) = getLocation ident
transformExpression (Value p (IntValue x)) = do 
  unless (minInt <= x && x <= maxInt) $ throwError $ IntegerOutOfBound x
  return $ ConstInt x
transformExpression (Value _ (BoolValue x)) = return $ ConstBool x
transformExpression (Value _ (StringValue x)) = return $ ConstString x
transformExpression (Application p ident expressions) = do
  locations <- mapM transformExpression expressions
  callFunction ident locations
transformExpression (Neg p expression) = do
  location <- transformExpression expression
  integerSub (ConstInt 0) location
transformExpression (Not p expression) = do
  location <- transformExpression expression
  boolNot location 
transformExpression (Operation p firstExpression op secondExpression) = do
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  case (getLocationType firstLocation, getLocationType secondLocation) of
    (Int, Int) -> transformBinaryOperation Int op firstLocation secondLocation
    (Bool, Bool) -> transformBinaryOperation Bool op firstLocation secondLocation
    (String, String) -> transformBinaryOperation String op firstLocation secondLocation
    (firstType, secondType) -> throwError $ TypeMissmatchBinaryOperator firstType secondType op
transformExpression (Compare p firstExpression op secondExpression) = do
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  case (getLocationType firstLocation, getLocationType secondLocation) of
    (Int, Int) -> integerCompare firstLocation op secondLocation
    (Bool, Bool) -> boolCompare firstLocation op secondLocation
    (String, String) -> stringCompare firstLocation op secondLocation
    (firstType, secondType) -> throwError $ TypeMissmatchCompare firstType secondType

transformDeclaration :: Type -> Declaration -> FunctionTransformer ()
transformDeclaration _type (NoInit p ident) = do
  defaultValue <- getDefaultValue _type
  newVariable _type ident $ ConstValue defaultValue
transformDeclaration _type (Init p ident expression) = do
  expressionLocation <- transformExpression expression
  let expressionType = getLocationType expressionLocation
  assertLocationType expressionLocation _type
  newVariable _type ident expressionLocation

transformStatement' :: Statement -> FunctionTransformer [BlockNumber]
transformStatement' (Empty _) = return []
transformStatement' (InnerBlock p block) = do
  previousBlockNumber <- getCurrentBlockNumber
  addJumpPlaceholder
  leaveQuadrupleBlock
  (newBlockNumber, finalBlocks) <- transformBlock block True
  addQuadrupleEdge previousBlockNumber newBlockNumber
  return $ makeFinalBlocks newBlockNumber finalBlocks
transformStatement' (Declaration _ _type declarations) = do
  mapM_ (transformDeclaration _type) declarations
  return []
transformStatement' (Assigment p ident expression) = do
  expressionLocation <- transformExpression expression
  let expressionType = getLocationType expressionLocation
  assertVariableType ident expressionType
  setLocation ident expressionLocation
  return []
transformStatement' (Increment p ident) = do
  assertVariableType ident Int
  location <- getLocation ident
  newLocation <- integerAdd location (ConstInt 1)
  setLocation ident newLocation
  return []
transformStatement' (Decrement p ident) = do
  assertVariableType ident Int
  location <- getLocation ident
  newLocation <- integerSub location (ConstInt 1)
  setLocation ident newLocation
  return []
transformStatement' (Return p expression) = do
  -- TODO what if return is not last
  expressionLocation <- transformExpression expression
  returnExpression expressionLocation
  blockNumber <- getCurrentBlockNumber
  leaveQuadrupleBlock
  return [blockNumber]
transformStatement' (VoidReturn p) = do
  -- TODO what if return is not last
  returnVoid
  blockNumber <- getCurrentBlockNumber
  leaveQuadrupleBlock
  return [blockNumber]
transformStatement' (If p expression statement) = do
  let dummyBlock = DummyBlock statement
  previousBlockNumber <- getCurrentBlockNumber
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  isAlive <- case expressionLocation of 
    (ConstBool x) -> return x
    _ -> return True
  (newBlockNumber, finalBlocks) <- transformBlock dummyBlock isAlive
  addQuadrupleEdge previousBlockNumber newBlockNumber
  return $ previousBlockNumber:(makeFinalBlocks newBlockNumber finalBlocks)
transformStatement' (IfElse p expression first second) = do
  let firstDummyBlock = DummyBlock first
  let secondDummyBlock = DummyBlock second
  previousBlockNumber <- getCurrentBlockNumber
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  (isFirstAlive, isSecondAlive) <- case expressionLocation of
    (ConstBool x) -> return (x, not x)
    _ -> return (True, True)
  (firstNumber, firstFinalBlocks) <- transformBlock firstDummyBlock isFirstAlive
  (secondNumber, secondFinalBlocks) <- transformBlock secondDummyBlock isSecondAlive
  addQuadrupleEdge previousBlockNumber firstNumber
  addQuadrupleEdge previousBlockNumber secondNumber
  return $ (makeFinalBlocks firstNumber firstFinalBlocks) ++ (makeFinalBlocks secondNumber secondFinalBlocks)
transformStatement' (While p expression statement) = do
  let dummyBlock = DummyBlock statement
  previousBlockNumber <- getCurrentBlockNumber
  addJumpPlaceholder
  leaveQuadrupleBlock
  conditionBlockNumber <- newQuadrupleBlock
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  isAlive <- case expressionLocation of
    (ConstBool x) -> return x
    _ -> return True
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  (loopBlockNumber, blocks) <- transformBlock dummyBlock isAlive
  addQuadrupleEdge previousBlockNumber conditionBlockNumber
  addQuadrupleEdge conditionBlockNumber loopBlockNumber
  mapM_ (\b -> addQuadrupleEdge b conditionBlockNumber) blocks
  return [conditionBlockNumber]
transformStatement' (Expression p expression) = do
  _ <- transformExpression expression
  return []

transformStatement :: [BlockNumber] -> Statement -> FunctionTransformer [BlockNumber]
transformStatement [] statement = transformStatement' statement
transformStatement finalBlocks statement = do
  assertNotInQuadrupleBlock
  newBlockNumber <- newQuadrupleBlock
  mapM_ (\s -> addQuadrupleEdge s newBlockNumber) finalBlocks
  transformStatement' statement

transformBlock :: Block -> Bool -> FunctionTransformer (BlockNumber, [BlockNumber])
transformBlock (Block p statements) isAlive = do
  newBlockNumber <- newQuadrupleBlock
  newScope
  finalBlocks <- foldM transformStatement [] statements
  leaveQuadrupleBlock
  removeScope
  return (newBlockNumber, makeFinalBlocks newBlockNumber finalBlocks)

transformFunction :: Function -> FunctionTransformer ()
transformFunction (Function p returnType _ arguments block) = do
  let (Block _ statements) = block
  newScope
  _ <- newQuadrupleBlock
  arguments <- gets $ view C.arguments
  mapM_ (uncurry argumentInit) (zip [0..] arguments)
  foldM_ transformStatement [] statements
  leaveQuadrupleBlock
  removeScope

transformFunctionToQuadruples :: Function -> GlobalTransformer ()
transformFunctionToQuadruples function = do
  result <- runFunctionTransformer transformFunction function
  return ()

transformProgram :: Program -> GlobalTransformer ()
transformProgram program@(Program p functions) = do
  defineGlobalSymbols program
  mapM_ transformFunctionToQuadruples functions
  assertMainExists

transformToQuadruples :: Program -> Either LatteError QuadruplesCode
transformToQuadruples program = case runGlobalTransformer transformProgram program of
    Left error -> Left error
    Right (_, code) -> Right code
-- 
-- Global context functions
-- 
getFunctionType :: Ident -> GlobalTransformer (Type, [Type])
getFunctionType ident = do
  maybeFunction <- Map.lookup ident . view Q.functions <$> get
  case maybeFunction of 
    Nothing -> throwError $ SymbolNotFound ident
    Just function -> do
      let returnType = view Q.returnType function
      let argumentTypes = map getArgumentType $ view Q.arguments function
      return (returnType, argumentTypes)

defineGlobalSymbols :: Program -> GlobalTransformer ()
defineGlobalSymbols (Program _ functions) = do
  mapM_ newSymbol libraryFunctions 
  mapM_ newSymbol functions

newSymbol :: Function -> GlobalTransformer ()
newSymbol (Function _ retType ident args _) = do
  assertCanDefineSymbol ident
  let newFunction = emptyFunction retType args
  modify $ over Q.functions (Map.insert ident newFunction)
-- 
-- Function context functions
-- 
getNewBlockNumber :: FunctionTransformer BlockNumber
getNewBlockNumber = do
  newBlockNumber <- gets $ view blockCounter
  modify $ over blockCounter (+1)
  return newBlockNumber

getNewRegisterNumber :: FunctionTransformer Index
getNewRegisterNumber = do
  newRegisterNumber <- gets $ view registerCounter
  modify $ over registerCounter (+1)
  return newRegisterNumber 

addQuadrupleEdge :: BlockNumber -> BlockNumber -> FunctionTransformer ()
addQuadrupleEdge source destination = do
  modifyBlock source $ over nextBlocks (destination:)
  modifyBlock destination $ over previousBlocks (source:)

newQuadrupleBlock :: FunctionTransformer BlockNumber
newQuadrupleBlock = do
  assertNotInQuadrupleBlock
  newBlockNumber <- getNewBlockNumber
  let newBlock = emptyBlockContext newBlockNumber
  modify $ over C.blocks (Map.insert newBlockNumber newBlock)
  setCurrentBlockNumber newBlockNumber
  scope <- getCurrentScope
  mapM_ addPhiPlaceholder scope
  return newBlockNumber

leaveQuadrupleBlock :: FunctionTransformer ()
leaveQuadrupleBlock = do
  scope <- getCurrentScope
  locations <- mapM getLocation scope
  modifyCurrentBlock $ set finalVariables (Map.fromList $ zip scope locations)
  modify $ set currentBlockNumber Nothing

addQuadrupleOperation :: QuadrupleOperation -> FunctionTransformer TemporaryRegister
addQuadrupleOperation operation = do
  newRegisterNumber <- getNewRegisterNumber
  let operationType = getOperationType operation
  let resultRegister = TemporaryRegister operationType newRegisterNumber
  let quadruple = Quadruple $ QuadrupleOperation resultRegister operation
  modifyCurrentBlock $ over code (quadruple:)
  return resultRegister

newVariable :: Type -> Ident -> Location -> FunctionTransformer ()
newVariable _type ident location = do
  assertCanDefineVariable ident
  modifyCurrentScope (ident:)
  modify $ over C.variables (Map.insertWith (++) ident [location])

removeVariable :: Ident -> FunctionTransformer ()
removeVariable ident = do
  maybeDefinitions <- Map.lookup ident . view variables <$> get
  case maybeDefinitions of
    Just (_:xs) -> modify $ over variables (Map.insert ident xs)
    _ -> throwError $ InternalCompilerError ("Variable " ++ ident ++ " not defined")

newScope :: FunctionTransformer ()
newScope = do
  modify $ over C.scopes ([]:)

removeScope :: FunctionTransformer ()
removeScope = do
  currentScope <- getCurrentScope
  mapM_ removeVariable currentScope
  modify $ over C.scopes tail
-- 
-- Getters, Setters and Modifiers
-- 
getCurrentBlockNumber :: FunctionTransformer BlockNumber
getCurrentBlockNumber = do
  maybeBlockNumber <- view currentBlockNumber <$> get
  case maybeBlockNumber of
    Nothing -> throwError $ InternalCompilerError "No block is specified as current"
    Just blockNumber -> return blockNumber

getCurrentScope :: FunctionTransformer [Ident]
getCurrentScope = do
  scopes <- view C.scopes <$> get
  case scopes of  
    [] -> throwError $ InternalCompilerError "Scope list is empty"
    (x:_) -> return x

getLocation :: Ident -> FunctionTransformer Location 
getLocation ident = do
  maybeInfo <- gets $ Map.lookup ident . view C.variables
  case maybeInfo of
    Nothing -> throwError $ SymbolNotFound ident
    Just [] -> throwError $ SymbolNotFound ident
    Just (x:_) -> return x

getBlock :: BlockNumber -> FunctionTransformer BlockContext  
getBlock blockNumber = do
  maybeBlock <- gets $ Map.lookup blockNumber . view C.blocks
  case maybeBlock of 
    Nothing -> throwError $ InternalCompilerError "Not able to find block with given block number"
    Just block -> return block

getCurrentBlock :: FunctionTransformer BlockContext    
getCurrentBlock = do
  currentBlockNumber <- getCurrentBlockNumber
  getBlock currentBlockNumber

setCurrentBlockNumber :: BlockNumber -> FunctionTransformer ()
setCurrentBlockNumber blockNumber = do
  modify $ set currentBlockNumber $ Just blockNumber

modifyCurrentScope :: ([Ident] -> [Ident]) -> FunctionTransformer ()  
modifyCurrentScope f = do
  scopes <- view C.scopes <$> get
  case scopes of
    [] -> throwError $ InternalCompilerError "Scope list is empty"
    (l:ls) -> modify $ set C.scopes (f l:ls)

setLocation :: Ident -> Location -> FunctionTransformer ()
setLocation ident location = do
  maybeInfo <- gets $ Map.lookup ident . view C.variables
  case maybeInfo of
    Nothing -> throwError $ SymbolNotFound ident
    Just [] -> throwError $ InternalCompilerError "Not able to find information about given variable" 
    Just (_:xs) -> modify $ over C.variables (Map.insert ident (location:xs))

modifyBlock :: BlockNumber -> (BlockContext -> BlockContext) -> FunctionTransformer ()
modifyBlock blockNumber f = do
  isMember <- gets $ Map.member blockNumber . view C.blocks
  case isMember of
    False -> throwError $ InternalCompilerError "Not able to find block with given block number"
    True -> modify $ over C.blocks (Map.update (Just . f) blockNumber)
  
modifyCurrentBlock :: (BlockContext -> BlockContext) -> FunctionTransformer ()
modifyCurrentBlock f = do
  currentBlockNumber <- getCurrentBlockNumber
  modifyBlock currentBlockNumber f
-- 
-- Assertions
-- 
assertMainExists :: GlobalTransformer ()
assertMainExists = do
  maybeMainFunction <- Map.lookup "main" . view Q.functions <$> get
  unless (isJust maybeMainFunction) (throwError $ SymbolNotFound "main")
  let mainFunction = fromJust maybeMainFunction
  let retType = view Q.returnType mainFunction
  let argTypes = map getArgumentType $ view Q.arguments mainFunction
  unless (retType == Int && null argTypes) (throwError $ SymbolNotFound "main")

assertCanDefineSymbol :: Ident -> GlobalTransformer ()
assertCanDefineSymbol ident = do
  canDefine <- not . Map.member ident . view Q.functions <$> get
  unless canDefine (throwError $ SymbolInScope ident)

assertNotInQuadrupleBlock :: FunctionTransformer ()
assertNotInQuadrupleBlock = do
  maybeBlockNumber <- view currentBlockNumber <$> get
  unless (isNothing maybeBlockNumber) $ throwError $ InternalCompilerError "Assertion: not in quadruple block"

assertCanDefineVariable :: Ident -> FunctionTransformer ()
assertCanDefineVariable ident = do
  isInScope <- elem ident <$> getCurrentScope
  unless (not isInScope) (throwError $ SymbolInScope ident)

assertReturnTypeIsCorrect :: Type -> FunctionTransformer ()
assertReturnTypeIsCorrect actualType = do
  expectedType <- gets $ view C.returnType
  function <- gets $ view C.functionName
  unless (actualType == expectedType) (throwError $ TypeMissmatchReturn function expectedType actualType)

assertLocationType :: Location -> Type -> FunctionTransformer ()
assertLocationType location actualType = do
  let expectedType = getLocationType location
  unless (expectedType == actualType) $ throwError $ TypeMissmatchAssigment expectedType actualType

assertVariableType :: Ident -> Type -> FunctionTransformer ()
assertVariableType ident actualType = do
  location <- getLocation ident
  assertLocationType location actualType

assertLocationIsBool :: Location -> FunctionTransformer ()
assertLocationIsBool location = do
  let locationType = getLocationType location
  unless (locationType == Bool) $ throwError $ TypeMissmatchIf locationType
-- 
-- Quadruples
-- 
addPhiPlaceholder :: Ident -> FunctionTransformer ()
addPhiPlaceholder ident = do
  let placeholder = PhiPlaceholder ident
  modifyCurrentBlock $ over code (placeholder:)

addJumpPlaceholder :: FunctionTransformer ()
addJumpPlaceholder = do
  modifyCurrentBlock $ over code (JumpPlaceholder:)

addConditionalJumpPlaceholder :: Location ->FunctionTransformer ()
addConditionalJumpPlaceholder location = do
  modifyCurrentBlock $ over code (ConditionalJumpPlaceholder location:)


argumentInit :: Index -> Argument -> FunctionTransformer ()
argumentInit index (Argument _type ident) = do
  let operation = ArgumentInit index _type
  register <- addQuadrupleOperation operation
  newVariable _type ident $ Register register 

integerAdd :: Location -> Location -> FunctionTransformer Location
integerAdd (ConstInt x) (ConstInt y) = return $ ConstInt (x + y)
integerAdd first second = do
  assertLocationType first Int
  assertLocationType second Int
  let minValue = min first second
  let maxValue = max first second
  let operation = IntegerAdd minValue maxValue
  Register <$> addQuadrupleOperation operation

integerSub :: Location -> Location -> FunctionTransformer Location
integerSub (ConstInt x) (ConstInt y) = return $ ConstInt (x - y)
integerSub first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = IntegerSub first second
  Register <$> addQuadrupleOperation operation

integerMul :: Location -> Location -> FunctionTransformer Location
integerMul (ConstInt x) (ConstInt y) = return $ ConstInt (x * y)
integerMul first second = do
  assertLocationType first Int
  assertLocationType second Int
  let minValue = min first second
  let maxValue = max first second
  let operation = IntegerMul minValue maxValue
  Register <$> addQuadrupleOperation operation

integerDiv :: Location -> Location -> FunctionTransformer Location
integerDiv (ConstInt x) (ConstInt y) = return $ ConstInt (div x  y)
integerDiv first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = IntegerDiv first second
  Register <$> addQuadrupleOperation operation

integerMod :: Location -> Location -> FunctionTransformer Location
integerMod (ConstInt x) (ConstInt y) = return $ ConstInt (mod x y)
integerMod first second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = IntegerMod first second
  Register <$> addQuadrupleOperation operation

boolAnd :: Location -> Location -> FunctionTransformer Location
boolAnd (ConstBool x) (ConstBool y) = return $ ConstBool (x && y)
boolAnd first second = do
  assertLocationType first Bool
  assertLocationType second Bool 
  let minValue = min first second
  let maxValue = max first second
  let operation = BoolAnd minValue maxValue
  Register <$> addQuadrupleOperation operation

boolOr :: Location -> Location -> FunctionTransformer Location
boolOr (ConstBool x) (ConstBool y) = return $ ConstBool (x || y)
boolOr first second = do
  assertLocationType first Bool
  assertLocationType second Bool 
  let minValue = min first second
  let maxValue = max first second
  let operation = BoolOr minValue maxValue
  Register <$> addQuadrupleOperation operation

boolNot :: Location -> FunctionTransformer Location
boolNot (ConstBool x) = return $ ConstBool (not x)
boolNot location = do
  assertLocationType location Bool
  let operation = BoolNot location
  Register <$> addQuadrupleOperation operation

stringConcat :: Location -> Location -> FunctionTransformer Location
stringConcat (ConstString x) (ConstString y) = return $ ConstString (x ++ y)
stringConcat first second = do
  assertLocationType first String
  assertLocationType second String
  let operation = StringConcat first second
  Register <$> addQuadrupleOperation operation 

integerCompare :: Location -> CompareOperation -> Location -> FunctionTransformer Location
integerCompare (ConstInt x) op (ConstInt y) = return $ ConstBool $ getCompareFunction op x y
integerCompare first op second = do
  assertLocationType first Int
  assertLocationType second Int
  let operation = IntegerCompare first op second
  Register <$> addQuadrupleOperation operation

stringCompare :: Location -> CompareOperation -> Location -> FunctionTransformer Location
stringCompare (ConstString x) op (ConstString y) = return $ ConstBool $ getCompareFunction op x y
stringCompare first op second = do
  assertLocationType first String
  assertLocationType second String 
  let operation = StringCompare first op second
  Register <$> addQuadrupleOperation operation

boolCompare :: Location -> CompareOperation -> Location -> FunctionTransformer Location
boolCompare (ConstBool x) op (ConstBool y) = return $ ConstBool $ getCompareFunction op x y
boolCompare first op second = do
  assertLocationType first String
  assertLocationType second String 
  let operation = BoolCompare first op second
  Register <$> addQuadrupleOperation operation

returnExpression :: Location -> FunctionTransformer ()
returnExpression location = do
  assertReturnTypeIsCorrect $ getLocationType location
  let operation = ReturnValue location
  void $ addQuadrupleOperation operation

returnVoid :: FunctionTransformer ()
returnVoid = do
  assertReturnTypeIsCorrect Void
  let operation = ReturnVoid
  void $ addQuadrupleOperation operation

callFunction :: Ident -> [Location] -> FunctionTransformer Location
callFunction ident locations = do
  let locationTypes = map getLocationType locations 
  (returnType, argumentTypes) <- lift $ getFunctionType ident
  unless (argumentTypes == locationTypes) $ throwError $ TypeMissmatchApplication ident argumentTypes locationTypes
  let operation = CallFunction ident returnType locations
  resultRegister <- addQuadrupleOperation operation
  return $ Register resultRegister

-- 
-- Other
-- 
getDefaultValue :: Type -> FunctionTransformer Value
getDefaultValue Int = return $ IntValue 0
getDefaultValue String = return $ StringValue ""
getDefaultValue Bool = return $ BoolValue False
getDefaultValue _type = throwError $ InternalCompilerError ("No default value for type " ++ show _type)

libraryFunctions :: [Function]
libraryFunctions = let 
    position = NoPosition 
    emptyBlock = Block position []
    createArg = \t -> Argument t "x"
  in [
    (Function position Void "printInt" [createArg Int] emptyBlock),
    (Function position Void "printString" [createArg String] emptyBlock),
    (Function position Void "error" [] emptyBlock), 
    (Function position (Int) "readInt" [] emptyBlock),
    (Function position (String) "readString" [] emptyBlock)
  ]

makeFinalBlocks :: BlockNumber -> [BlockNumber] -> [BlockNumber]
makeFinalBlocks block [] = [block]
makeFinalBlocks _ blocks = blocks
-- 
-- Transformers
-- 
runGlobalTransformer :: (Program -> GlobalTransformer ()) -> Program -> Either LatteError ((), QuadruplesCode)
runGlobalTransformer transformer program = let
    initialState = emptyGlobalContext 
  in
    runStateT (transformer program) initialState

runFunctionTransformer :: (Function -> FunctionTransformer ()) -> Function -> GlobalTransformer ((), FunctionContext)
runFunctionTransformer transformer function = let
    (Function _ returnType ident arguments _) = function
    initialState = emptyFunctionContext returnType ident arguments 
  in
    runStateT (transformer function) initialState
--
-- Patterns
-- 
pattern DummyBlock :: Statement -> Block
pattern DummyBlock statement = Block NoPosition [statement]