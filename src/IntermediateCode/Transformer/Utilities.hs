module IntermediateCode.Transformer.Utilities where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Errors
import Data.Maybe
import Control.Monad
import Types
import qualified IntermediateCode.Definitions.AbstractSyntaxTree as AST
import qualified IntermediateCode.Definitions.Quadruples as Q 
import qualified IntermediateCode.Transformer.Context as C
import Lens.Micro.Platform
import Debug.Trace

-- 
-- Global context functions
--
getFunctionType :: Ident -> C.GlobalTransformer (Maybe (Type, [Type]))
getFunctionType ident = do
  maybeFunction <- Map.lookup ident . view Q.functions <$> get
  case maybeFunction of 
    -- Nothing -> throwError $ (SymbolNotFound ident, NoPosition)
    Nothing -> return Nothing
    Just function -> do
      let returnType = view Q.returnType function
      let argumentTypes = map getType $ view Q.arguments function
      return $ Just (returnType, argumentTypes)

defineGlobalSymbols :: [AST.GlobalSymbol] -> C.GlobalTransformer ()
defineGlobalSymbols globalSymbols = do
  mapM_ newGlobalSymbol libraryFunctions 
  mapM_ newGlobalSymbol globalSymbols

newGlobalSymbol :: AST.GlobalSymbol -> C.GlobalTransformer ()
newGlobalSymbol (AST.Function _ retType ident args _) = do
  assertCanDefineFunction ident
  let newFunction = Q.emptyFunction retType args
  modify $ over Q.functions (Map.insert ident newFunction)
-- 
-- Function context functions
-- 
savePosition :: Position -> C.FunctionTransformer ()
savePosition position = modify $ set C.position position

throwLatteError :: LatteError -> C.FunctionTransformer a
throwLatteError latteError = do
  position <- view C.position <$> get
  throwError $ (latteError, position)

getNewBlockNumber :: C.FunctionTransformer Q.BlockNumber
getNewBlockNumber = do
  newBlockNumber <- gets $ view C.blockCounter
  modify $ over C.blockCounter (+1)
  return newBlockNumber

getNewRegisterNumber :: C.FunctionTransformer Index
getNewRegisterNumber = do
  newRegisterNumber <- gets $ view C.registerCounter
  modify $ over C.registerCounter (+1)
  return newRegisterNumber

newVariable :: Type -> Ident -> Q.QuadrupleLocation -> C.FunctionTransformer ()
newVariable _type ident location = do
  assertCanDefineVariable ident
  modifyCurrentScope (ident:)
  modify $ over C.variables (Map.insertWith (++) ident [location])

removeVariable :: Ident -> C.FunctionTransformer ()
removeVariable ident = do
  maybeDefinitions <- Map.lookup ident . view C.variables <$> get
  case maybeDefinitions of
    Just (_:xs) -> modify $ over C.variables (Map.insert ident xs)
    _ -> throwLatteError $ InternalCompilerError ("Variable " ++ ident ++ " not defined")

newScope :: C.FunctionTransformer ()
newScope = do
  modify $ over C.scopes ([]:)

removeScope :: C.FunctionTransformer ()
removeScope = do
  currentScope <- getCurrentScope
  mapM_ removeVariable currentScope
  modify $ over C.scopes tail
-- 
-- Getters, Setters and Modifiers
-- 
getCurrentBlockNumber :: C.FunctionTransformer Q.BlockNumber
getCurrentBlockNumber = do
  maybeBlockNumber <- view C.currentBlockNumber <$> get
  case maybeBlockNumber of
    Nothing -> throwLatteError $ InternalCompilerError "No block is specified as current"
    Just blockNumber -> return blockNumber

getCurrentScope :: C.FunctionTransformer [Ident]
getCurrentScope = do
  scopes <- view C.scopes <$> get
  case scopes of  
    [] -> throwLatteError $ InternalCompilerError "Scope list is empty"
    (x:_) -> return x

getLocation :: Ident -> C.FunctionTransformer Q.QuadrupleLocation 
getLocation ident = do
  maybeInfo <- gets $ Map.lookup ident . view C.variables
  case maybeInfo of
    Nothing -> throwLatteError $ SymbolNotFound ident
    Just [] -> throwLatteError $ SymbolNotFound ident
    Just (x:_) -> return x

getBlock :: Q.BlockNumber -> C.FunctionTransformer C.BlockContext  
getBlock blockNumber = do
  maybeBlock <- gets $ Map.lookup blockNumber . view C.blocks
  case maybeBlock of 
    Nothing -> throwLatteError $ InternalCompilerError "Not able to find block with given block number"
    Just block -> return block

getCurrentBlock :: C.FunctionTransformer C.BlockContext    
getCurrentBlock = do
  currentBlockNumber <- getCurrentBlockNumber
  getBlock currentBlockNumber

setCurrentBlockNumber :: Q.BlockNumber -> C.FunctionTransformer ()
setCurrentBlockNumber blockNumber = do
  modify $ set C.currentBlockNumber $ Just blockNumber

modifyCurrentScope :: ([Ident] -> [Ident]) -> C.FunctionTransformer ()  
modifyCurrentScope f = do
  scopes <- view C.scopes <$> get
  case scopes of
    [] -> throwLatteError $ InternalCompilerError "Scope list is empty"
    (l:ls) -> modify $ set C.scopes (f l:ls)

setLocation :: Ident -> Q.QuadrupleLocation -> C.FunctionTransformer ()
setLocation ident location = do
  maybeInfo <- gets $ Map.lookup ident . view C.variables
  case maybeInfo of
    Nothing -> throwLatteError $ SymbolNotFound ident
    Just [] -> throwLatteError $ InternalCompilerError "Not able to find information about given variable" 
    Just (_:xs) -> modify $ over C.variables (Map.insert ident (location:xs))

modifyBlock :: Q.BlockNumber -> (C.BlockContext -> C.BlockContext) -> C.FunctionTransformer ()
modifyBlock blockNumber f = do
  isMember <- gets $ Map.member blockNumber . view C.blocks
  case isMember of
    False -> throwLatteError $ InternalCompilerError "Not able to find block with given block number"
    True -> modify $ over C.blocks (Map.update (Just . f) blockNumber)
  
modifyCurrentBlock :: (C.BlockContext -> C.BlockContext) -> C.FunctionTransformer ()
modifyCurrentBlock f = do
  currentBlockNumber <- getCurrentBlockNumber
  modifyBlock currentBlockNumber f
-- 
-- Assertions
-- 
assertMainExists :: C.GlobalTransformer ()
assertMainExists = do
  maybeMainFunction <- Map.lookup "main" . view Q.functions <$> get
  unless (isJust maybeMainFunction) (throwError $ (SymbolNotFound "main", NoPosition))
  let mainFunction = fromJust maybeMainFunction
  let retType = view Q.returnType mainFunction
  let argTypes = map getType $ view Q.arguments mainFunction
  unless (retType == Int && null argTypes) (throwError $ (SymbolNotFound "main", NoPosition))

assertCanDefineFunction :: Ident -> C.GlobalTransformer ()
assertCanDefineFunction ident = do
  canDefine <- not . Map.member ident . view Q.functions <$> get
  unless canDefine (throwError $ (SymbolInScope ident, NoPosition))

assertNotInQuadrupleBlock :: C.FunctionTransformer ()
assertNotInQuadrupleBlock = do
  maybeBlockNumber <- view C.currentBlockNumber <$> get
  unless (isNothing maybeBlockNumber) $ throwLatteError $ InternalCompilerError "Assertion: not in quadruple block"

assertCanDefineVariable :: Ident -> C.FunctionTransformer ()
assertCanDefineVariable ident = do
  isInScope <- elem ident <$> getCurrentScope
  unless (not isInScope) (throwLatteError $ SymbolInScope ident)

assertReturnTypeIsCorrect :: Type -> C.FunctionTransformer ()
assertReturnTypeIsCorrect actualType = do
  expectedType <- gets $ view C.returnType
  function <- gets $ view C.functionIdent
  unless (actualType == expectedType) (throwLatteError $ TypeMissmatchReturn function expectedType actualType)

assertLocationType :: Q.QuadrupleLocation -> Type -> C.FunctionTransformer ()
assertLocationType location actualType = do
  let expectedType = getType location
  unless (expectedType == actualType) $ throwLatteError $ TypeMissmatchAssigment expectedType actualType

assertVariableType :: Ident -> Type -> C.FunctionTransformer ()
assertVariableType ident actualType = do
  location <- getLocation ident
  assertLocationType location actualType

assertLocationIsBool :: Q.QuadrupleLocation -> C.FunctionTransformer ()
assertLocationIsBool location = do
  let locationType = getType location
  unless (locationType == Bool) $ throwLatteError $ TypeMissmatchIf locationType

assertFinalBlocksHaveReturn :: C.FunctionTransformer ()
assertFinalBlocksHaveReturn = do
  returnType <- view C.returnType <$> get
  functionIdent <- view C.functionIdent <$> get
  blocks <- Map.elems . view C.blocks <$> get
  savedFinalBlockNumbers <- view C.finalBlocks <$> get
  savedFinalBlocks <- mapM getBlock savedFinalBlockNumbers
  let aliveBlocks = filter (view C.isAlive) $ traceShowId blocks
  let finalBlocks = filter (null . view C.nextBlocks) $ traceShowId aliveBlocks
  let invalidBlocks = filter (not . view C.hasReturn) $ traceShowId finalBlocks
  case returnType of
    Void -> return ()
    _ -> do
      let latteError = MissingReturn  functionIdent returnType 
      unless (null invalidBlocks) $ throwLatteError $ InternalCompilerError "A"
      unless (not $ null finalBlocks) $ throwLatteError $ InternalCompilerError "B"

assertFinalBlocksHaveReturn2 :: C.FunctionTransformer ()
assertFinalBlocksHaveReturn2 = do
  returnType <- view C.returnType <$> get
  functionIdent <- view C.functionIdent <$> get
  savedFinalBlockNumbers <- view C.finalBlocks <$> get
  savedFinalBlocks <- mapM getBlock $ traceShowId savedFinalBlockNumbers
  let aliveBlocks = filter (view C.isAlive) $ traceShowId savedFinalBlocks
  let invalidBlocks = filter (not . view C.hasReturn) $ traceShowId aliveBlocks
  case returnType of
    Void -> return ()
    _ -> do
      let latteError = MissingReturn  functionIdent returnType 
      unless (null invalidBlocks) $ throwLatteError $ InternalCompilerError "A"
-- 
-- Other
-- 
getDefaultConstValue :: Type -> C.FunctionTransformer Q.QuadrupleLocation
getDefaultConstValue Int = return $ Q.ConstInt 0
getDefaultConstValue String = return $ Q.ConstString ""
getDefaultConstValue Bool = return $ Q.ConstBool False
getDefaultConstValue _type = throwLatteError $ InternalCompilerError ("No default value for type " ++ show _type)

libraryFunctions :: [AST.GlobalSymbol]
libraryFunctions = let 
    position = NoPosition 
    emptyBlock = AST.Block position []
    createArg = \t -> Argument t "x"
  in [
    (AST.Function position Void "printInt" [createArg Int] emptyBlock),
    (AST.Function position Void "printString" [createArg String] emptyBlock),
    (AST.Function position Void "error" [] emptyBlock), 
    (AST.Function position (Int) "readInt" [] emptyBlock),
    (AST.Function position (String) "readString" [] emptyBlock)
  ]

singletonIfEmpty :: [a] -> a -> [a]
singletonIfEmpty [] x = [x]
singletonIfEmpty list _ = list