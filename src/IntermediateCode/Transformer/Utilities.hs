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
modifyClass :: Ident -> (C.ClassDefinition -> C.ClassDefinition) -> C.GlobalTransformer ()
modifyClass ident fun = modify $ over C.classes (Map.update (Just . fun) ident)

getClass :: Ident -> C.GlobalTransformer C.ClassDefinition
getClass classIdent = do
  maybeClass <- Map.lookup classIdent . view C.classes <$> get
  case maybeClass of
    Nothing -> throwErrorGlobal $ SymbolNotFound classIdent
    Just classDefinition -> return classDefinition

getClassMemberIndex :: Ident -> Ident -> C.GlobalTransformer Index
getClassMemberIndex classIdent memberIdent = do
  classDefinition <- getClass classIdent
  let maybeIndex = Map.lookup memberIdent $ view C.attributes classDefinition
  case maybeIndex of 
    Nothing -> throwErrorGlobal $ ClassMemberNotFound classIdent memberIdent
    Just index -> return index

getClassMemberType :: Ident -> Ident -> C.GlobalTransformer Type
getClassMemberType classIdent memberIdent = do
  classDefinition <- getClass classIdent
  let maybeType = Map.lookup memberIdent $ view C.attributeTypes  classDefinition
  case maybeType of 
    Nothing -> throwErrorGlobal $ ClassMemberNotFound classIdent memberIdent
    Just _type -> return _type

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
  assertCorrectTypesInGlobalSymbols

addClassMember :: Ident -> (Index, AST.ClassMember) -> C.GlobalTransformer ()
addClassMember classIdent (index, AST.AttributeDefinition _ _type memberIdent) = do
  modifyClass classIdent $ over C.attributes $ Map.insert memberIdent index
  modifyClass classIdent $ over C.attributeTypes $ Map.insert memberIdent _type

newGlobalSymbol :: AST.GlobalSymbol -> C.GlobalTransformer ()
newGlobalSymbol (AST.Function _ retType ident args _) = do
  assertCanDefineFunction ident
  let newFunction = Q.emptyFunctionDefinition ident retType args
  modify $ over (C.quadruples . Q.functions) (Map.insert ident newFunction)
newGlobalSymbol (AST.Class _ ident members) = do
  assertCanDefineClass ident
  let returnType = Object ident
  let constructorIdent = getConstructorIdent ident
  let emptyClassDefinition = C.ClassDefinition Map.empty Map.empty
  let constructor = Q.emptyFunctionDefinition constructorIdent returnType []
  modify $ over (C.quadruples . Q.functions) (Map.insert constructorIdent constructor)
  modify $ over C.classes (Map.insert ident emptyClassDefinition)
  mapM_ (addClassMember ident) $ zip [0..] members
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

isLocationAnArray :: Q.QuadrupleLocation -> C.FunctionTransformer Bool
isLocationAnArray location = do
  locationType <- getLocationType location
  case locationType of
    (Array _) -> return True
    _ -> return False
    
isLocationAnObject :: Q.QuadrupleLocation -> C.FunctionTransformer Bool
isLocationAnObject location = do
  locationType <- getLocationType location
  case locationType of
    (Object _) -> return True
    _ -> return False
-- 
-- Getters, Setters and Modifiers
--
getArrayValueType :: Q.QuadrupleLocation -> C.FunctionTransformer Type
getArrayValueType location = do
  locationType <- getLocationType location
  case locationType of 
    Array _type -> return _type
    _ -> throwErrorFunction $ NotAnArray locationType

getMemberIndex :: Q.QuadrupleLocation -> Ident -> C.FunctionTransformer Index
getMemberIndex location memberIdent = do
  locationType <- getLocationType location
  case locationType of
    Object classIdent -> lift $ getClassMemberIndex classIdent memberIdent
    _ -> throwErrorFunction $ NotAnObject locationType

getMemberType :: Q.QuadrupleLocation -> Ident -> C.FunctionTransformer Type
getMemberType location memberIdent = do
  locationType <- getLocationType location
  case locationType of
    Object classIdent -> lift $ getClassMemberType classIdent memberIdent
    _ -> throwErrorFunction $ NotAnObject locationType 

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
  canDefine <- not . Map.member ident . view (C.quadruples . Q.functions) <$> get
  unless canDefine (throwError $ (SymbolInScope ident, NoPosition))

assertCanDefineClass :: Ident -> C.GlobalTransformer ()
assertCanDefineClass ident = do
  canDefine <- not . Map.member ident . view C.classes <$> get
  unless canDefine $ throwError $ (SymbolInScope ident, NoPosition)

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

assertIsArray :: Q.QuadrupleLocation -> C.FunctionTransformer ()
assertIsArray location = do
  locationType <- getLocationType location
  case locationType of
    Array _ -> return ()
    _ -> throwErrorFunction $ NotAnArray locationType

assertArrayValueType :: Q.QuadrupleLocation -> Type -> C.FunctionTransformer ()
assertArrayValueType location expectedType = do
  locationType <- getLocationType location
  let latteError = TypeMissmatch expectedType locationType
  case locationType of
    Array actualType -> unless (actualType == expectedType) $ throwErrorFunction latteError
    _ -> throwErrorFunction latteError

assertIsObject :: Q.QuadrupleLocation -> C.FunctionTransformer ()
assertIsObject location = do
  locationType <- getLocationType location
  case locationType of
    Object _ -> return ()
    _ -> throwErrorFunction $ NotAnObject locationType

assertObjectType :: Q.QuadrupleLocation -> Ident -> C.FunctionTransformer ()
assertObjectType location requiredIdent = do 
  locationType <- getLocationType location
  let latteError = TypeMissmatch (Object requiredIdent) locationType
  case locationType of
    Object actualIdent -> unless (actualIdent == requiredIdent) $ throwErrorFunction latteError
    _ -> throwErrorFunction latteError

assertLocationType :: Q.QuadrupleLocation -> Type -> C.FunctionTransformer ()
assertLocationType location expectedType = do
  actualType <- getLocationType location
  case (actualType, expectedType) of
    (Array _, Int) -> return ()
    (Object _, Int) -> return ()
    (Int, Array _) -> return ()
    (Int, Object _) -> return ()
    otherwise -> unless (expectedType == actualType) $ throwErrorFunction $ TypeMissmatchAssigment expectedType actualType

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
      assertFinalBlocksHaveReturnFinalBlocks

assertIsCorrectType :: Type -> C.GlobalTransformer ()
assertIsCorrectType (Object ident) = do
  isDefined <- Map.member ident . view C.classes <$> get
  unless (traceShowId isDefined) $ throwErrorGlobal $ SymbolNotFound ident
assertIsCorrectType _ = return ()

assertCorrectTypesInFunction :: Q.FunctionDefinition -> C.GlobalTransformer ()
assertCorrectTypesInFunction functionDefinition = do
  let types = map getType $ view Q.arguments functionDefinition
  mapM_ assertIsCorrectType types

assertCorrectTypesInClass :: C.ClassDefinition -> C.GlobalTransformer ()
assertCorrectTypesInClass classDefinition = do
  let types = Map.elems $ view C.attributeTypes classDefinition
  mapM_ assertIsCorrectType types

assertCorrectTypesInGlobalSymbols :: C.GlobalTransformer ()
assertCorrectTypesInGlobalSymbols = do
   Map.elems . view (C.quadruples . Q.functions) <$> get >>= mapM_ assertCorrectTypesInFunction
   Map.elems . view C.classes <$> get >>= mapM_ assertCorrectTypesInClass 

assertClassExists :: Ident -> C.GlobalTransformer ()
assertClassExists classIdent = do
  isMember <- Map.member classIdent . view C.classes <$> get
  case isMember of
    True -> return ()
    False -> throwErrorGlobal $ SymbolNotFound classIdent
-- 
-- Other
-- 
getDefaultValue :: Type -> C.FunctionTransformer Q.QuadrupleLocation
getDefaultValue Int = return $ Q.ConstInt 0
getDefaultValue String = return $ Q.ConstString ""
getDefaultValue Bool = return $ Q.ConstBool False
getDefaultValue (Array _) = return $ Q.ConstInt 0
getDefaultValue (Object _) = return $ Q.ConstInt 0
getDefaultValue _type = throwErrorFunction $ InternalCompilerError ("No default value for type " ++ show _type)

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
    (AST.Function position Int "readInt" [] emptyBlock),
    (AST.Function position String "readString" [] emptyBlock),
    (AST.Function position Int "__createObject" [createArg Int] emptyBlock),
    (AST.Function position Int "__createArray" [createArg Int] emptyBlock)
  ]

singletonIfEmpty :: [a] -> a -> [a]
singletonIfEmpty [] x = [x]
singletonIfEmpty list _ = list

getConstructorIdent :: Ident -> Ident
getConstructorIdent ident = "__constructor_" ++ ident

constructorCode :: Int -> [Q.Quadruple]
constructorCode size = [
    Q.QuadrupleOperation 0 $ Q.CallFunction "__createObject" [Q.ConstInt size],
    Q.QuadrupleOperation 1 $ Q.ReturnValue $ Q.Register 0
  ]