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
throwErrorGlobal :: LatteError -> C.GlobalTransformer a
throwErrorGlobal latteError = do
  position <- view C.position <$> get
  throwError (latteError, position)

getFunctionType :: Ident -> C.GlobalTransformer (Type, [Type])
getFunctionType ident = do
  maybeFunction <- Map.lookup ident . view (C.quadruples . Q.functions) <$> get
  case maybeFunction of 
    Nothing -> throwErrorGlobal $ SymbolNotFound ident
    Just function -> do
      let returnType = view Q.returnType function
      let argumentTypes = map getType $ view Q.arguments function
      return (returnType, argumentTypes)

defineGlobalSymbols :: [AST.GlobalSymbol] -> C.GlobalTransformer ()
defineGlobalSymbols globalSymbols = do
  mapM_ newGlobalSymbol libraryFunctions 
  mapM_ newGlobalSymbol globalSymbols

newGlobalSymbol :: AST.GlobalSymbol -> C.GlobalTransformer ()
newGlobalSymbol (AST.Function _ retType ident args _) = do
  assertCanDefineFunction ident
  let newFunction = Q.emptyFunctionDefinition ident retType args
  modify $ over (C.quadruples . Q.functions) (Map.insert ident newFunction)
-- 
-- Function context functions
-- 
savePosition :: Position -> C.GlobalTransformer ()
savePosition position = modify $ set C.position position

throwErrorFunction :: LatteError -> C.FunctionTransformer a
throwErrorFunction latteError = lift $ throwErrorGlobal latteError

getNewDummyNumber :: C.FunctionTransformer Int
getNewDummyNumber = do
  newDummyNumber <- view C.dummyCounter <$> get
  modify $ over C.dummyCounter (+1)
  return newDummyNumber

getNewDummyIdent :: C.FunctionTransformer Ident
getNewDummyIdent = do
  dummyIdent <- (++) "__tmp_" . show <$> getNewDummyNumber
  isMember <- Map.member dummyIdent . view C.variables <$> get
  case isMember of
    False -> return dummyIdent
    True -> getNewDummyIdent

getNewBlockNumber :: C.FunctionTransformer Q.BlockNumber
getNewBlockNumber = do
  newBlockNumber <- gets $ view C.blockCounter
  modify $ over C.blockCounter (+1)
  return newBlockNumber

getNewRegisterNumber :: Type -> C.FunctionTransformer Index
getNewRegisterNumber _type = do
  newRegisterNumber <- gets $ view C.registerCounter
  modify $ over C.registerCounter (+1)
  modify $ over C.locationTypes (Map.insert newRegisterNumber _type)
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
    _ -> throwErrorFunction $ InternalCompilerError ("Variable " ++ ident ++ " not defined")

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
getLocationType :: Q.QuadrupleLocation -> C.FunctionTransformer Type
getLocationType (Q.ConstInt _) = return Int
getLocationType (Q.ConstBool _) = return Bool
getLocationType (Q.ConstString _) = return String
getLocationType (Q.Register register) = do
  maybeType <- Map.lookup register . view C.locationTypes <$> get
  case maybeType of
    Nothing -> throwErrorFunction $ InternalCompilerError "Type is not specified for given type"
    Just _type -> return _type

getCurrentBlockNumber :: C.FunctionTransformer Q.BlockNumber
getCurrentBlockNumber = do
  maybeBlockNumber <- view C.currentBlockNumber <$> get
  case maybeBlockNumber of
    Nothing -> throwErrorFunction $ InternalCompilerError "No block is specified as current"
    Just blockNumber -> return blockNumber

getCurrentScope :: C.FunctionTransformer [Ident]
getCurrentScope = do
  scopes <- view C.scopes <$> get
  case scopes of  
    [] -> throwErrorFunction $ InternalCompilerError "Scope list is empty"
    (x:_) -> return x

getLocation :: Ident -> C.FunctionTransformer Q.QuadrupleLocation 
getLocation ident = do
  maybeInfo <- gets $ Map.lookup ident . view C.variables
  case maybeInfo of
    Nothing -> throwErrorFunction $ SymbolNotFound ident
    Just [] -> throwErrorFunction $ SymbolNotFound ident
    Just (x:_) -> return x

getBlock :: Q.BlockNumber -> C.FunctionTransformer C.BlockContext  
getBlock blockNumber = do
  maybeBlock <- gets $ Map.lookup blockNumber . view C.blocks
  case maybeBlock of 
    Nothing -> throwErrorFunction $ InternalCompilerError "Not able to find block with given block number"
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
    [] -> throwErrorFunction $ InternalCompilerError "Scope list is empty"
    (l:ls) -> modify $ set C.scopes (f l:ls)

setLocation :: Ident -> Q.QuadrupleLocation -> C.FunctionTransformer ()
setLocation ident location = do
  maybeInfo <- gets $ Map.lookup ident . view C.variables
  case maybeInfo of
    Nothing -> throwErrorFunction $ SymbolNotFound ident
    Just [] -> throwErrorFunction $ InternalCompilerError "Not able to find information about given variable" 
    Just (_:xs) -> modify $ over C.variables (Map.insert ident (location:xs))

modifyBlock :: Q.BlockNumber -> (C.BlockContext -> C.BlockContext) -> C.FunctionTransformer ()
modifyBlock blockNumber f = do
  isMember <- gets $ Map.member blockNumber . view C.blocks
  case isMember of
    False -> throwErrorFunction $ InternalCompilerError "Not able to find block with given block number"
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
  (returnType, argumentTypes) <- getFunctionType "main"
  unless (returnType == Int && null argumentTypes) (throwError $ (SymbolNotFound "main", NoPosition))

assertCanDefineFunction :: Ident -> C.GlobalTransformer ()
assertCanDefineFunction ident = do
  canDefine <- not . Map.member ident . view (C.quadruples .Q.functions) <$> get
  unless canDefine (throwError $ (SymbolInScope ident, NoPosition))

assertNotInQuadrupleBlock :: C.FunctionTransformer ()
assertNotInQuadrupleBlock = do
  maybeBlockNumber <- view C.currentBlockNumber <$> get
  unless (isNothing maybeBlockNumber) $ throwErrorFunction $ InternalCompilerError "Assertion: not in quadruple block"

assertCanDefineVariable :: Ident -> C.FunctionTransformer ()
assertCanDefineVariable ident = do
  isInScope <- elem ident <$> getCurrentScope
  unless (not isInScope) (throwErrorFunction $ SymbolInScope ident)

assertReturnTypeIsCorrect :: Type -> C.FunctionTransformer ()
assertReturnTypeIsCorrect actualType = do
  expectedType <- gets $ view C.returnType
  function <- gets $ view C.functionIdent
  unless (actualType == expectedType) (throwErrorFunction $ TypeMissmatchReturn function expectedType actualType)

assertLocationType :: Q.QuadrupleLocation -> Type -> C.FunctionTransformer ()
assertLocationType location expectedType = do
  actualType <- getLocationType location
  unless (expectedType == actualType) $ throwErrorFunction $ TypeMissmatchAssigment expectedType actualType

assertVariableType :: Ident -> Type -> C.FunctionTransformer ()
assertVariableType ident actualType = do
  location <- getLocation ident
  assertLocationType location actualType

assertLocationIsBool :: Q.QuadrupleLocation -> C.FunctionTransformer ()
assertLocationIsBool location = do
  actualType <- getLocationType location
  unless (actualType == Bool) $ throwErrorFunction $ TypeMissmatchIf actualType

assertFinalBlocksHaveReturnPath :: C.FunctionTransformer ()
assertFinalBlocksHaveReturnPath = do
  returnType <- view C.returnType <$> get
  functionIdent <- view C.functionIdent <$> get
  blocks <- Map.elems . view C.blocks <$> get
  let aliveBlocks = filter (view C.isAlive) blocks
  let finalBlocks = filter (null . view C.nextBlocks) aliveBlocks
  let invalidBlocks = filter (not . view C.hasReturn) finalBlocks 
  let latteError = MissingReturn functionIdent returnType
  unless (null invalidBlocks) $ throwErrorFunction $ latteError
  unless (not $ null finalBlocks) $ throwErrorFunction $ latteError

assertFinalBlocksHaveReturnFinalBlocks :: C.FunctionTransformer ()
assertFinalBlocksHaveReturnFinalBlocks = do
  returnType <- view C.returnType <$> get
  functionIdent <- view C.functionIdent <$> get
  savedFinalBlockNumbers <- view C.finalBlocks <$> get
  savedFinalBlocks <- mapM getBlock savedFinalBlockNumbers
  let aliveBlocks = filter (view C.isAlive) savedFinalBlocks
  let invalidBlocks = filter (not . view C.hasReturn) aliveBlocks
  unless (null invalidBlocks) $ throwErrorFunction $ MissingReturn  functionIdent returnType 

assertFinalBlocksHaveReturn :: C.FunctionTransformer ()
assertFinalBlocksHaveReturn = do
  returnType <- view C.returnType <$> get
  case returnType of
    Void -> return ()
    _ -> do
      -- assertFinalBlocksHaveReturnPath
      assertFinalBlocksHaveReturnFinalBlocks
-- 
-- Other
-- 
getDefaultConstValue :: Type -> C.FunctionTransformer Q.QuadrupleLocation
getDefaultConstValue Int = return $ Q.ConstInt 0
getDefaultConstValue String = return $ Q.ConstString ""
getDefaultConstValue Bool = return $ Q.ConstBool False
getDefaultConstValue _type = throwErrorFunction $ InternalCompilerError ("No default value for type " ++ show _type)

libraryFunctions :: [AST.GlobalSymbol]
libraryFunctions = let 
    position = NoPosition 
    emptyBlock = AST.Block position []
    createArg = \t -> Argument t "x"
  in [
    (AST.Function position Int "debugGcsePrintInt" [createArg Int] emptyBlock),
    (AST.Function position Void "printInt" [createArg Int] emptyBlock),
    (AST.Function position Void "printString" [createArg String] emptyBlock),
    (AST.Function position Void "error" [] emptyBlock), 
    (AST.Function position (Int) "readInt" [] emptyBlock),
    (AST.Function position (String) "readString" [] emptyBlock)
  ]

singletonIfEmpty :: [a] -> a -> [a]
singletonIfEmpty [] x = [x]
singletonIfEmpty list _ = list