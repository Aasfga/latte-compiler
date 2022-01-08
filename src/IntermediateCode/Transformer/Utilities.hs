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
getFunctionType :: Ident -> C.GlobalTransformer (Type, [Type])
getFunctionType ident = do
  maybeFunction <- Map.lookup ident . view Q.functions <$> get
  case maybeFunction of 
    Nothing -> throwError $ SymbolNotFound ident
    Just function -> do
      let returnType = view Q.returnType function
      let argumentTypes = map getType $ view Q.arguments function
      return (returnType, argumentTypes)

defineGlobalSymbols :: [AST.GlobalSymbol] -> C.GlobalTransformer ()
defineGlobalSymbols globalSymbols = do
  mapM_ newSymbol libraryFunctions 
  mapM_ newSymbol globalSymbols

newSymbol :: AST.GlobalSymbol -> C.GlobalTransformer ()
newSymbol (AST.Function _ retType ident args _) = do
  assertCanDefineSymbol ident
  let newFunction = Q.emptyFunction retType args
  modify $ over Q.functions (Map.insert ident newFunction)
-- 
-- Function context functions
-- 
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
    _ -> throwError $ InternalCompilerError ("Variable " ++ ident ++ " not defined")

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
    Nothing -> throwError $ InternalCompilerError "No block is specified as current"
    Just blockNumber -> return blockNumber

getCurrentScope :: C.FunctionTransformer [Ident]
getCurrentScope = do
  scopes <- view C.scopes <$> get
  arguments <- map getIdent . view C.arguments <$> get
  case scopes of  
    [] -> throwError $ InternalCompilerError "Scope list is empty"
    [x] -> return $ arguments ++ x
    (x:_) -> return x

getLocation :: Ident -> C.FunctionTransformer Q.QuadrupleLocation 
getLocation ident = do
  maybeInfo <- gets $ Map.lookup ident . view C.variables
  case maybeInfo of
    Nothing -> throwError $ SymbolNotFound ident
    Just [] -> throwError $ SymbolNotFound ident
    Just (x:_) -> return x

getBlock :: Q.BlockNumber -> C.FunctionTransformer C.BlockContext  
getBlock blockNumber = do
  maybeBlock <- gets $ Map.lookup blockNumber . view C.blocks
  case maybeBlock of 
    Nothing -> throwError $ InternalCompilerError "Not able to find block with given block number"
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
    [] -> throwError $ InternalCompilerError "Scope list is empty"
    (l:ls) -> modify $ set C.scopes (f l:ls)

setLocation :: Ident -> Q.QuadrupleLocation -> C.FunctionTransformer ()
setLocation ident location = do
  maybeInfo <- gets $ Map.lookup ident . view C.variables
  case maybeInfo of
    Nothing -> throwError $ SymbolNotFound ident
    Just [] -> throwError $ InternalCompilerError "Not able to find information about given variable" 
    Just (_:xs) -> modify $ over C.variables (Map.insert ident (location:xs))

modifyBlock :: Q.BlockNumber -> (C.BlockContext -> C.BlockContext) -> C.FunctionTransformer ()
modifyBlock blockNumber f = do
  isMember <- gets $ Map.member blockNumber . view C.blocks
  case isMember of
    False -> throwError $ InternalCompilerError "Not able to find block with given block number"
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
  unless (isJust maybeMainFunction) (throwError $ SymbolNotFound "main")
  let mainFunction = fromJust maybeMainFunction
  let retType = view Q.returnType mainFunction
  let argTypes = map getType $ view Q.arguments mainFunction
  unless (retType == Int && null argTypes) (throwError $ SymbolNotFound "main")

assertCanDefineSymbol :: Ident -> C.GlobalTransformer ()
assertCanDefineSymbol ident = do
  canDefine <- not . Map.member ident . view Q.functions <$> get
  unless canDefine (throwError $ SymbolInScope ident)

assertNotInQuadrupleBlock :: C.FunctionTransformer ()
assertNotInQuadrupleBlock = do
  maybeBlockNumber <- view C.currentBlockNumber <$> get
  unless (isNothing maybeBlockNumber) $ throwError $ InternalCompilerError "Assertion: not in quadruple block"

assertCanDefineVariable :: Ident -> C.FunctionTransformer ()
assertCanDefineVariable ident = do
  isInScope <- elem ident <$> getCurrentScope
  unless (not isInScope) (throwError $ SymbolInScope ident)

assertReturnTypeIsCorrect :: Type -> C.FunctionTransformer ()
assertReturnTypeIsCorrect actualType = do
  expectedType <- gets $ view C.returnType
  function <- gets $ view C.functionIdent
  unless (actualType == expectedType) (throwError $ TypeMissmatchReturn function expectedType actualType)

assertLocationType :: Q.QuadrupleLocation -> Type -> C.FunctionTransformer ()
assertLocationType location actualType = do
  let expectedType = getType location
  unless (expectedType == actualType) $ throwError $ TypeMissmatchAssigment expectedType actualType

assertVariableType :: Ident -> Type -> C.FunctionTransformer ()
assertVariableType ident actualType = do
  location <- getLocation ident
  assertLocationType location actualType

assertLocationIsBool :: Q.QuadrupleLocation -> C.FunctionTransformer ()
assertLocationIsBool location = do
  let locationType = getType location
  unless (locationType == Bool) $ throwError $ TypeMissmatchIf locationType

assertFinalBlocksHaveReturn :: [(Q.BlockNumber, Bool)] -> C.FunctionTransformer ()
assertFinalBlocksHaveReturn blocks = do
  functionIdent <- view C.functionIdent <$> get
  returnType <- view C.returnType <$> get
  when (returnType == Void) $ return ()
  let aliveBlocks = map fst $ filter snd blocks
  blocks <- mapM getBlock aliveBlocks
  let allHaveReturn = all (view $ C.hasReturn) blocks
  unless (allHaveReturn) $ throwError $ MissingReturn functionIdent returnType 
-- 
-- Other
-- 
getDefaultConstValue :: Type -> C.FunctionTransformer Q.QuadrupleLocation
getDefaultConstValue Int = return $ Q.ConstInt 0
getDefaultConstValue String = return $ Q.ConstString ""
getDefaultConstValue Bool = return $ Q.ConstBool False
getDefaultConstValue _type = throwError $ InternalCompilerError ("No default value for type " ++ show _type)

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

makeFinalBlocks :: (Q.BlockNumber, Bool) -> [(Q.BlockNumber, Bool)] -> [(Q.BlockNumber, Bool)]
makeFinalBlocks block [] = [block]
makeFinalBlocks _ blocks = blocks