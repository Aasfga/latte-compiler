
{-# LANGUAGE PatternSynonyms #-}
module IntermediateCode.Transformer where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Except
import Errors
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Types
import qualified IntermediateCode.Definitions.AbstractSyntaxTree as AST
import qualified IntermediateCode.Definitions.Quadruples as Q 
import qualified IntermediateCode.Transformer.Context as C
import Lens.Micro.Platform
import Debug.Trace
import IntermediateCode.Transformer.Utilities
import IntermediateCode.Transformer.Operations


-- 
-- Transformer
-- 
transformBinaryOperation :: Type -> Operation -> Q.QuadrupleLocation -> Q.QuadrupleLocation -> C.FunctionTransformer Q.QuadrupleLocation
transformBinaryOperation Int Plus = integerAdd
transformBinaryOperation Int Minus = integerSub
transformBinaryOperation Int Times = integerMul
transformBinaryOperation Int Div = integerDiv
transformBinaryOperation Int Mod = integerMod
transformBinaryOperation Bool And = boolAnd
transformBinaryOperation Bool Or = boolOr
transformBinaryOperation String Plus = stringConcat
transformBinaryOperation _type op = (\_ _ -> throwErrorFunction $ TypeMissmatchBinaryOperator _type _type op)

transformExpression' :: AST.Expression -> C.FunctionTransformer Q.QuadrupleLocation 
transformExpression' (AST.Variable _ ident) = getLocation ident
transformExpression' (AST.Value _ (AST.IntegerValue x)) = do 
  let minValue = toInteger (minBound :: Int)
  let maxValue = toInteger (maxBound :: Int)
  unless (minValue < x && x < maxValue) $ throwErrorFunction $ IntegerOutOfBound x
  return $ Q.ConstInt $ fromIntegral x
transformExpression' (AST.Value _ (AST.BoolValue x)) = return $ Q.ConstBool x
transformExpression' (AST.Value _ (AST.StringValue x)) = return $ Q.ConstString x
transformExpression' (AST.Application _ ident expressions) = do
  locations <- mapM transformExpression expressions
  callFunction ident locations
transformExpression' (AST.Neg _ expression) = do
  location <- transformExpression expression
  integerSub (Q.ConstInt 0) location
transformExpression' (AST.Not _ expression) = do
  location <- transformExpression expression
  boolNot location 
transformExpression' (AST.Operation _ firstExpression And secondExpression) = do
  dummyIdent <- getNewDummyIdent
  isAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  newScope
  newVariable Bool dummyIdent (Q.ConstBool False)
  let ifAssigment = AST.DummyAssigment dummyIdent secondExpression
  let elseAssigment = AST.DummyAssigment dummyIdent $ AST.DummyBool False
  let statement = AST.DummyIfElse firstExpression ifAssigment elseAssigment
  leaveQuadrupleBlock Q.Jump
  (newBlockNumber, result) <- transformBlock (AST.DummyBlock statement) isAlive
  transformStatement (Just result) AST.DummyEmpty
  location <- getLocation dummyIdent
  removeScope
  addQuadrupleEdge currentBlockNumber newBlockNumber
  return location
transformExpression' (AST.Operation _ firstExpression Or secondExpression) = do
  dummyIdent <- getNewDummyIdent
  isAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  newScope
  newVariable Bool dummyIdent (Q.ConstBool False)
  let ifAssigment = AST.DummyAssigment dummyIdent $ AST.DummyBool True
  let elseAssigment = AST.DummyAssigment dummyIdent secondExpression
  let statement = AST.DummyIfElse firstExpression ifAssigment elseAssigment
  leaveQuadrupleBlock Q.Jump
  (newBlockNumber, result) <- transformBlock (AST.DummyBlock statement) isAlive
  transformStatement (Just result) AST.DummyEmpty
  location <- getLocation dummyIdent
  removeScope
  addQuadrupleEdge currentBlockNumber newBlockNumber
  return location
transformExpression' (AST.Operation _ firstExpression op secondExpression) = do
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  firstLocationType <- getLocationType firstLocation
  secondLocationType <- getLocationType secondLocation
  case (firstLocationType, secondLocationType) of
    (Int, Int) -> transformBinaryOperation Int op firstLocation secondLocation
    (Bool, Bool) -> transformBinaryOperation Bool op firstLocation secondLocation
    (String, String) -> transformBinaryOperation String op firstLocation secondLocation
    (firstType, secondType) -> throwErrorFunction $ TypeMissmatchBinaryOperator firstType secondType op
transformExpression' (AST.Compare _ firstExpression op secondExpression) = do
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  firstLocationType <- getLocationType firstLocation
  secondLocationType <- getLocationType secondLocation
  case (firstLocationType, secondLocationType) of
    (Int, Int) -> integerCompare firstLocation op secondLocation
    (Bool, Bool) -> boolCompare firstLocation op secondLocation
    (String, String) -> stringCompare firstLocation op secondLocation
    (firstType, secondType) -> throwErrorFunction $ TypeMissmatchCompare firstType secondType

transformExpression :: AST.Expression -> C.FunctionTransformer Q.QuadrupleLocation
transformExpression expression = do
  let position = getPosition expression
  when (position /= NoPosition) $ lift $ savePosition position
  transformExpression' expression

transformDeclaration :: Type -> AST.Declaration -> C.FunctionTransformer ()
transformDeclaration _type (AST.NoInit p ident) = do
  lift $ savePosition p
  constValue <- getDefaultConstValue _type
  newVariable _type ident constValue
transformDeclaration _type (AST.Init p ident expression) = do
  lift $ savePosition p
  expressionLocation <- transformExpression expression
  expressionType <- getLocationType expressionLocation
  assertLocationType expressionLocation _type
  newVariable _type ident expressionLocation

type StatementReturn = ([Q.BlockNumber], Bool)

defaultStatementReturn :: C.FunctionTransformer (Maybe StatementReturn)
defaultStatementReturn = do
  isAlive <- view C.isAlive <$> getCurrentBlock
  blockNumber <- getCurrentBlockNumber
  return $ Just ([blockNumber], isAlive)

transformStatement' :: AST.Statement -> C.FunctionTransformer (Maybe StatementReturn)
transformStatement' (AST.Empty _) = defaultStatementReturn
transformStatement' (AST.InnerBlock _ block) = do
  currentAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  leaveQuadrupleBlock Q.Jump
  (newBlockNumber, result) <- transformBlock block currentAlive
  addQuadrupleEdge currentBlockNumber newBlockNumber
  return $ Just result
transformStatement' (AST.Declaration _ _type declarations) = do
  mapM_ (transformDeclaration _type) declarations
  defaultStatementReturn
transformStatement' (AST.Assigment _ ident expression) = do
  expressionLocation <- transformExpression expression
  expressionType <- getLocationType expressionLocation
  assertVariableType ident expressionType
  setLocation ident expressionLocation
  defaultStatementReturn
transformStatement' (AST.Increment _ ident) = do
  assertVariableType ident Int
  location <- getLocation ident
  newLocation <- integerAdd location (Q.ConstInt 1)
  setLocation ident newLocation
  defaultStatementReturn
transformStatement' (AST.Decrement _ ident) = do
  assertVariableType ident Int
  location <- getLocation ident
  newLocation <- integerSub location (Q.ConstInt 1)
  setLocation ident newLocation
  defaultStatementReturn
transformStatement' (AST.Return _ expression) = do
  expressionLocation <- transformExpression expression
  currentBlockNumber <- getCurrentBlockNumber
  returnExpression expressionLocation
  modifyCurrentBlock $ set C.hasReturn True
  leaveQuadrupleBlock Q.Return
  return Nothing
transformStatement' (AST.VoidReturn _) = do
  currentBlockNumber <- getCurrentBlockNumber
  returnVoid
  modifyCurrentBlock $ set C.hasReturn True
  leaveQuadrupleBlock Q.Return
  return Nothing
transformStatement' (AST.If _ expression statement) = do
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  let dummyBlock = AST.DummyBlock statement
  currentAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  (isIfAlive, maybeValue) <- case expressionLocation of 
    (Q.ConstBool value) -> return $ (currentAlive && value, Just value)
    _ -> return (currentAlive, Nothing)
  when (isNothing maybeValue) $ leaveQuadrupleBlock $ Q.ConditionalJump expressionLocation
  when (isJust maybeValue) $ leaveQuadrupleBlock $ Q.Jump
  (newBlockNumber, result) <- transformBlock dummyBlock isIfAlive
  let (ifFinals, ifFinalsAlive) = result
  when (isNothing maybeValue || fromJust maybeValue) $ addQuadrupleEdge currentBlockNumber newBlockNumber
  return $ Just $ case (maybeValue) of
    Just True -> (ifFinals, ifFinalsAlive)
    Just False -> ([currentBlockNumber], currentAlive)
    Nothing -> (currentBlockNumber:ifFinals, currentAlive || ifFinalsAlive)
transformStatement' (AST.IfElse _ expression ifStatement elseStatement) = do
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  let ifBlock = AST.DummyBlock ifStatement
  let elseBlock = AST.DummyBlock elseStatement
  currentAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  leaveQuadrupleBlock $ Q.ConditionalJump expressionLocation
  (ifAlive, elseAlive) <- case expressionLocation of
    (Q.ConstBool x) -> return (currentAlive && x, currentAlive && (not x))
    _ -> return (currentAlive, currentAlive)
  (ifBlockNumber, (ifFinals, ifFinalsAlive)) <- transformBlock ifBlock ifAlive
  (elseBlockNumber, (elseFinals, elseFinalsAlive)) <- transformBlock elseBlock elseAlive
  addQuadrupleEdge currentBlockNumber ifBlockNumber
  addQuadrupleEdge currentBlockNumber elseBlockNumber 
  return $ Just (ifFinals ++ elseFinals, ifAlive || elseAlive)
transformStatement' (AST.While _ expression statement) = do
  let dummyBlock = AST.DummyBlock statement
  currentAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  leaveQuadrupleBlock Q.Jump
  beginConditionBlockNumber <- newQuadrupleBlock currentAlive
  expressionLocation <- transformExpression expression
  finalConditionBlockNumber <- getCurrentBlockNumber
  assertLocationIsBool expressionLocation
  (isWhileAlive, maybeValue) <- case expressionLocation of
    (Q.ConstBool value) -> return $ (currentAlive && value, Just value)
    _ -> return (currentAlive, Nothing)
  leaveQuadrupleBlock $ Q.ConditionalJump expressionLocation
  (loopBlockNumber, (loopFinals, loopFinalsAlive)) <- transformBlock dummyBlock currentAlive
  addQuadrupleEdge currentBlockNumber beginConditionBlockNumber
  addQuadrupleEdge finalConditionBlockNumber loopBlockNumber
  mapM_ (\s -> addQuadrupleEdge s beginConditionBlockNumber) loopFinals
  case maybeValue of
    Just True -> return Nothing
    _ -> return $ Just ([finalConditionBlockNumber], currentAlive)
transformStatement' (AST.Expression _ expression) = do
  _ <- transformExpression expression
  defaultStatementReturn

transformStatement :: (Maybe StatementReturn) -> AST.Statement -> C.FunctionTransformer (Maybe StatementReturn)
transformStatement statementReturn statement = do
  let position = getPosition statement
  when (position /= NoPosition) $ lift $ savePosition position
  blockNumber <- fromMaybe (-1) . view C.currentBlockNumber <$> get
  case statementReturn of
    Nothing -> do
      assertNotInQuadrupleBlock
      void $ newQuadrupleBlock False
    Just ([], _) -> return ()
    Just (finalBlocks@(first:_), finalBlocksAlive) -> do
      case first == blockNumber && length finalBlocks == 1 of
        True -> return ()
        False -> do
          assertNotInQuadrupleBlock
          newBlockNumber <- newQuadrupleBlock finalBlocksAlive
          mapM_ (\s -> addQuadrupleEdge s newBlockNumber) finalBlocks
  transformStatement' statement

transformBlock :: AST.Block -> Bool -> C.FunctionTransformer (Q.BlockNumber, StatementReturn)
transformBlock (AST.Block p statements) isAlive = do
  lift $ savePosition p
  newBlockNumber <- newQuadrupleBlock isAlive
  newScope
  result <- foldM transformStatement (Just ([], isAlive)) statements
  removeScope
  case result of 
    Nothing -> do
      dummyBlockNumber <- newQuadrupleBlock False
      leaveQuadrupleBlock Q.None
      return (newBlockNumber, ([dummyBlockNumber], False))
    Just (stmtFinalBlocks, stmtFinalBlocksAlive) -> do
      finalBlocks <- case stmtFinalBlocks of
        [] -> (:[]) <$> getCurrentBlockNumber
        list -> return list
      gentlyLeaveQuadrupleBlock Q.None
      return (newBlockNumber, (finalBlocks, stmtFinalBlocksAlive))

transformFunctionBlock :: AST.Block -> C.FunctionTransformer ()
transformFunctionBlock (AST.Block p statements) = do
  lift $ savePosition p
  newScope
  firstBlock <- newQuadrupleBlock True
  modifyCurrentBlock $ set C.isAlive True
  arguments <- gets $ view C.arguments
  mapM_ (uncurry argumentInit) (zip [0..] arguments)
  result <- foldM transformStatement (Just ([firstBlock], True)) statements
  case result of 
    Just (finalBlocks, _) -> modify $ set C.finalBlocks finalBlocks
    _ -> return ()
  gentlyLeaveQuadrupleBlock Q.None
  removeScope

transformFunction :: AST.Block -> C.FunctionTransformer Q.FunctionDefinition
transformFunction block = do
  transformFunctionBlock block
  returnType <- view C.returnType <$> get
  when (returnType == Void) addDummyReturnVoidBlock
  removeEdgesFromReturningBlocks
  assertFinalBlocksHaveReturn
  addDummyJumps
  parseFunctionContext
  
transformGlobalSymbolToQuadruples :: AST.GlobalSymbol -> C.GlobalTransformer ()
transformGlobalSymbolToQuadruples (AST.Function _ returnType functionName arguments block) = do
  (functionDefinition,_) <- runFunctionTransformer returnType functionName arguments block
  modify $ over (C.quadruples . Q.functions) (Map.insert functionName functionDefinition)

transformProgram :: AST.Program -> C.GlobalTransformer Q.Quadruples
transformProgram (AST.Program _ globalSymbols) = do
  defineGlobalSymbols globalSymbols
  mapM_ transformGlobalSymbolToQuadruples globalSymbols
  assertMainExists
  view C.quadruples <$> get

transformToQuadruples :: AST.Program -> Either CompilerError Q.Quadruples
transformToQuadruples program = case runGlobalTransformer program of
    Left (error, position) -> Left $ CompilerError error position
    Right (quadruples, _) -> let
        functions = view Q.functions quadruples
        noLibraryFunctions = foldl (flip Map.delete) functions $ map getIdent libraryFunctions
        newQuadruples = set Q.functions noLibraryFunctions quadruples
      in
        Right newQuadruples
-- 
-- Finishers
--
addDummyJumps :: C.FunctionTransformer ()
addDummyJumps = do
  blockNumbers <- Map.keys . view C.blocks <$> get
  mapM_ (\b -> do
      nextBlocks <- view C.nextBlocks <$> getBlock b
      when (length nextBlocks == 1) $ modifyBlock b $ set C.finalOperation Q.Jump
    ) blockNumbers

parsePreQuadruple :: C.PreQuadruple -> C.FunctionTransformer Q.Quadruple
parsePreQuadruple (C.Quadruple quadruple) = return quadruple

parseBlockContext :: C.BlockContext -> C.FunctionTransformer Q.Block
parseBlockContext blockContext = do
  let blockNumber = view C.blockNumber blockContext
  let phiVariables = view C.phiVariables blockContext
  let finalVariables = view C.finalVariables blockContext
  let previousBlocks = view C.previousBlocks blockContext
  let nextBlocks = view C.nextBlocks blockContext
  let isAlive = view C.isAlive blockContext
  let preCode = view C.code blockContext
  let finalOperation = view C.finalOperation blockContext
  code <- reverse <$> mapM parsePreQuadruple preCode
  let hasReturn = view C.hasReturn blockContext
  return $ Q.Block blockNumber phiVariables Set.empty finalVariables previousBlocks nextBlocks isAlive code hasReturn finalOperation

parseFunctionContext :: C.FunctionTransformer Q.FunctionDefinition
parseFunctionContext = do
  functionContext <- get
  let functionIdent = view C.functionIdent functionContext
  let returnType = view C.returnType functionContext
  let arguments = view C.arguments functionContext
  let registerCounter = view C.registerCounter functionContext
  let preBlocks = view C.blocks functionContext
  blocks <- mapM parseBlockContext preBlocks
  return $ Q.FunctionDefinition functionIdent returnType arguments registerCounter blocks

removeEdgesFromReturningBlocks :: C.FunctionTransformer ()
removeEdgesFromReturningBlocks = do
  blocks <- Map.elems . view C.blocks <$> get
  let returningBlocks = filter (view C.hasReturn) blocks
  let tuples = map (\b -> (view C.blockNumber b, view C.nextBlocks b)) returningBlocks
  mapM_ (\(blockNumber, nextBlocks) -> mapM (removeQuadrupleEdge blockNumber) nextBlocks) tuples

addDummyReturnVoidBlock :: C.FunctionTransformer ()
addDummyReturnVoidBlock = do
  let statement = AST.VoidReturn NoPosition
  let block = AST.DummyBlock statement
  finalBlocks <- view C.finalBlocks <$> get
  (lastBlockNumber, _) <- transformBlock block True
  mapM_ (\s -> addQuadrupleEdge s lastBlockNumber) finalBlocks
-- 
-- Runners
-- 
runGlobalTransformer :: AST.Program -> Either (LatteError, Position) (Q.Quadruples, C.GlobalContext)
runGlobalTransformer program = let
    initialState = C.emptyGlobalContext 
  in
    runStateT (transformProgram program) initialState

runFunctionTransformer :: Type -> Ident -> [Argument] -> AST.Block -> C.GlobalTransformer (Q.FunctionDefinition, C.FunctionContext)
runFunctionTransformer returnType functionIdent arguments block = let
    initialState = C.emptyFunctionContext returnType functionIdent arguments 
  in
    runStateT (transformFunction block) initialState
-- 
-- Debug
-- 
checkBlock :: C.BlockContext -> C.FunctionTransformer ()
checkBlock block = do
  let blockNumber = view C.blockNumber block
  finalBlocks <- traceShowId . view C.finalBlocks <$> get
  unless (blockNumber < 10000) $ throwErrorFunction $ InternalCompilerError "xyz"

checkBlocks :: C.FunctionTransformer ()
checkBlocks = do
  blocks <- Map.elems . view C.blocks <$> get
  mapM_ (checkBlock . traceShowId) blocks
  finalBlocks <- view C.finalBlocks <$> get 
  when (trace ("finalBlocks=" ++ show finalBlocks) False) $ throwErrorFunction $ InternalCompilerError "xyz"