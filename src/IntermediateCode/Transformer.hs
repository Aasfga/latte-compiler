
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
transformDeclaration :: Type -> Declaration -> FunctionTransformer ()
transformDeclaration _type (NoInit p ident) = do
  defaultValue <- getDefaultValue _type
  defineVariable _type ident $ Q.Value defaultValue

transformStatement :: Statement -> FunctionTransformer ()
transformStatement (Empty _) = return ()
transformStatement (InnerBlock _ (Block _ statements)) = do
  previousBlockNumber <- gets $ view currentBlockNumber
  newBlockNumber <- newQuadrupleBlock
  addQuadrupleEdge previousBlockNumber newBlockNumber
  scope <- gets $ head . view C.scope
  mapM_ addPhiPlaceholder scope
  newScope
  mapM_ transformStatement statements
  removeScope
transformStatement (Declaration _ _type declarations) = do
  mapM_ (transformDeclaration _type) declarations

transformBlock :: Block -> FunctionTransformer ()
transformBlock (Block p statements) = do
  arguments <- gets $ view C.arguments
  mapM_ (uncurry argInit) (zip [0..] arguments)
  newScope
  mapM_ transformStatement statements
  removeScope

transformFunction :: Function -> FunctionTransformer ()
transformFunction (Function p returnType ident arguments block) = do
  transformBlock block

transformFunctionToQuadruples :: Function -> GlobalTransformer ()
transformFunctionToQuadruples function = do
  result <- runFunctionTransformer transformFunction function
  return ()

defineGlobalSymbols :: Program -> GlobalTransformer ()
defineGlobalSymbols (Program _ functions) = do
  mapM_ defineSymbol libraryFunctions 
  mapM_ defineSymbol functions

transformProgram :: Program -> GlobalTransformer ()
transformProgram program = do
  defineGlobalSymbols program
  assertMainExists

transformToQuadruples :: Program -> Either LatteError QuadruplesCode
transformToQuadruples program = case runGlobalTransformer transformProgram program of
    Left error -> Left error
    Right (_, code) -> Right code
-- 
-- Global context functions
-- 
assertMainExists :: GlobalTransformer ()
assertMainExists = do
  maybeFunction <- gets $ Map.lookup "main" . view Q.functions
  unless (isJust maybeFunction) (throwError $ SymbolNotFound "main")
  let function = fromJust maybeFunction
  let retType = view Q.returnType function
  let argTypes = map getArgumentType $ view Q.arguments function
  unless (retType == Int && null argTypes) (throwError $ SymbolNotFound "main")

assertCanDefineSymbol :: Ident -> GlobalTransformer ()
assertCanDefineSymbol ident = do
  functionExists <- gets $ Map.member ident . view Q.functions
  let canDefine = not functionExists
  unless canDefine (throwError $ SymbolInScope ident)

defineSymbol :: Function -> GlobalTransformer ()
defineSymbol (Function _ retType ident args _) = do
  assertCanDefineSymbol ident
  let newFunction = emptyFunction retType args
  modify $ over Q.functions (Map.insert ident newFunction)

-- Function context functions
getNewBlockNumber :: FunctionTransformer BlockNumber
getNewBlockNumber = do
  newBlockNumber <- gets $ view blockCounter
  modify $ over blockCounter (+1)
  return newBlockNumber

getNewRegisterNumber :: FunctionTransformer TemporaryRegister
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
  newBlockNumber <- getNewBlockNumber
  let newBlock = emptyBlockContext newBlockNumber
  modify $ over C.blocks (Map.insert newBlockNumber newBlock)
  modify $ set currentBlockNumber newBlockNumber
  return newBlockNumber

addQuadrupleOperation :: QuadrupleOperation -> FunctionTransformer TemporaryRegister
addQuadrupleOperation operation = do
  newRegisterNumber <- getNewRegisterNumber
  let quadruple = Quadruple $ QuadrupleOperation newRegisterNumber operation
  let operationType = getOperationType operation
  modifyCurrentBlock $ over code (quadruple:)
  modify $ over resultTypes (Map.insert newRegisterNumber operationType)
  return newRegisterNumber

defineVariable :: Type -> Ident -> VariableLocation  -> FunctionTransformer ()
defineVariable _type ident variableLocation = do
  let variableInfo = VariableInfo _type variableLocation
  assertCanDefineVariable ident
  assertScopeDefined
  modify $ over C.scope (\(l:ls) -> (ident:l):ls)
  modify $ over C.variables (Map.insertWith (++) ident [variableInfo])

newScope :: FunctionTransformer ()
newScope = do
  modify $ over C.scope ([]:)

removeScope :: FunctionTransformer ()
removeScope = do
  assertScopeDefined
  modify $ over C.scope tail

getBlock :: BlockNumber -> FunctionTransformer BlockContext  
getBlock blockNumber = do
  maybeBlock <- gets $ Map.lookup blockNumber . view C.blocks
  case maybeBlock of 
    Nothing -> throwError $ InternalCompilerError "Not able to find block with given block number"
    Just block -> return block

getCurrentBlock :: FunctionTransformer BlockContext    
getCurrentBlock = do
  currentBlockNumber <- gets $ view C.currentBlockNumber
  getBlock currentBlockNumber

modifyBlock :: BlockNumber -> (BlockContext -> BlockContext) -> FunctionTransformer ()
modifyBlock blockNumber f = do
  isMember <- gets $ Map.member blockNumber . view C.blocks
  case isMember of
    False -> throwError $ InternalCompilerError "Not able to find block with given block number"
    True -> modify $ over C.blocks (Map.update (Just . f) $ blockNumber)
  
modifyCurrentBlock :: (BlockContext -> BlockContext) -> FunctionTransformer ()
modifyCurrentBlock f = do
  currentBlockNumber <- gets $ view C.currentBlockNumber
  modifyBlock currentBlockNumber f

assertScopeDefined :: FunctionTransformer ()
assertScopeDefined = do
  isEmpty <- gets $ null . view C.scope
  unless (not isEmpty) (throwError $ InternalCompilerError "Scope list is empty")

assertCanDefineVariable :: Ident -> FunctionTransformer ()
assertCanDefineVariable ident = do
  isInScope <- gets $ (elem ident) . head . view C.scope
  unless (not isInScope) (throwError $ SymbolInScope ident)

assertReturnTypeIsCorrect :: Type -> FunctionTransformer ()
assertReturnTypeIsCorrect actualType = do
  expectedType <- gets $ view C.returnType
  unless (actualType == expectedType) (throwError $ TypeMissmatchReturn "fun" expectedType actualType)
-- 
-- Quadruples
-- 
argInit :: Index -> Argument -> FunctionTransformer ()
argInit index (Argument _type ident) = do
  let operation = ARG_INIT index _type
  register <- addQuadrupleOperation operation
  defineVariable _type ident $ TemporaryRegister register

addPhiPlaceholder :: Ident -> FunctionTransformer ()
addPhiPlaceholder ident = do
  let placeholder = PhiPlaceholder ident
  modifyCurrentBlock $ over code (placeholder:)
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