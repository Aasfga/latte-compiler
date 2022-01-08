module IntermediateCode.Transformer.Utilities where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Errors
import Data.Maybe
import Control.Monad
import Types
import IntermediateCode.Definitions.AbstractSyntaxTree as AST
import IntermediateCode.Definitions.Quadruples as Q 
import IntermediateCode.Transformer.TransformerContext as C
import Lens.Micro.Platform
import Debug.Trace

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
newSymbol (AST.Function _ retType ident args _) = do
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
  function <- gets $ view C.functionIdent
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

assertFinalBlocksHaveReturn :: [(BlockNumber, Bool)] -> FunctionTransformer ()
assertFinalBlocksHaveReturn blocks = do
  functionIdent <- view C.functionIdent <$> get
  returnType <- view C.returnType <$> get
  when (returnType == Void) $ return ()
  let aliveBlocks = map fst $ filter snd blocks
  blocks <- mapM getBlock aliveBlocks
  let allHaveReturn = all (view $ hasReturn) blocks
  unless (allHaveReturn) $ throwError $ MissingReturn functionIdent returnType 
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
    (AST.Function position Void "printInt" [createArg Int] emptyBlock),
    (AST.Function position Void "printString" [createArg String] emptyBlock),
    (AST.Function position Void "error" [] emptyBlock), 
    (AST.Function position (Int) "readInt" [] emptyBlock),
    (AST.Function position (String) "readString" [] emptyBlock)
  ]

makeFinalBlocks :: (BlockNumber, Bool) -> [(BlockNumber, Bool)] -> [(BlockNumber, Bool)]
makeFinalBlocks block [] = [block]
makeFinalBlocks _ blocks = blocks