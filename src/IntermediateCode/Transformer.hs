
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
  leaveQuadrupleBlock
  (newBlockNumber, result) <- transformBlock (AST.DummyBlock statement) isAlive
  transformStatement result AST.DummyEmpty
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
  leaveQuadrupleBlock
  (newBlockNumber, result) <- transformBlock (AST.DummyBlock statement) isAlive
  transformStatement result AST.DummyEmpty
  location <- getLocation dummyIdent
  removeScope
  addQuadrupleEdge currentBlockNumber newBlockNumber
  return location
transformExpression' (AST.Operation _ firstExpression op secondExpression) = do
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  case (getType firstLocation, getType secondLocation) of
    (Int, Int) -> transformBinaryOperation Int op firstLocation secondLocation
    (Bool, Bool) -> transformBinaryOperation Bool op firstLocation secondLocation
    (String, String) -> transformBinaryOperation String op firstLocation secondLocation
    (firstType, secondType) -> throwErrorFunction $ TypeMissmatchBinaryOperator firstType secondType op
transformExpression' (AST.Compare _ firstExpression op secondExpression) = do
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  case (getType firstLocation, getType secondLocation) of
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
  let expressionType = getType expressionLocation
  assertLocationType expressionLocation _type
  newVariable _type ident expressionLocation

type StatementReturn = ([Q.BlockNumber], Bool)

defaultStatementReturn :: C.FunctionTransformer StatementReturn
defaultStatementReturn = do
  currentAlive <- view C.isAlive <$> getCurrentBlock
  return ([], currentAlive)

transformStatement' :: AST.Statement -> C.FunctionTransformer StatementReturn
transformStatement' (AST.Empty _) = return ([], True)
transformStatement' (AST.InnerBlock _ block) = do
  currentAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  addJumpPlaceholder
  leaveQuadrupleBlock 
  (newBlockNumber, result) <- transformBlock block currentAlive
  let (finalBlocks, areFinalBlocksAlive) = result
  addQuadrupleEdge currentBlockNumber newBlockNumber
  return result
transformStatement' (AST.Declaration _ _type declarations) = do
  mapM_ (transformDeclaration _type) declarations
  defaultStatementReturn
transformStatement' (AST.Assigment _ ident expression) = do
  expressionLocation <- transformExpression expression
  let expressionType = getType expressionLocation
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
  leaveQuadrupleBlock
  return ([currentBlockNumber], False)
transformStatement' (AST.VoidReturn _) = do
  currentBlockNumber <- getCurrentBlockNumber
  returnVoid
  modifyCurrentBlock $ set C.hasReturn True
  leaveQuadrupleBlock
  return ([currentBlockNumber], False)
transformStatement' (AST.If _ expression statement) = do
  let dummyBlock = AST.DummyBlock statement
  currentAlive <- view C.isAlive <$> getCurrentBlock
  expressionLocation <- transformExpression expression
  currentBlockNumber <- getCurrentBlockNumber
  assertLocationIsBool expressionLocation
  (isIfAlive, cond) <- case expressionLocation of 
    (Q.ConstBool x) -> return $ (currentAlive && x, x)
    _ -> return (currentAlive, False)
  when (not cond) $ addConditionalJumpPlaceholder expressionLocation
  when (cond) $ addJumpPlaceholder
  leaveQuadrupleBlock
  (newBlockNumber, result) <- transformBlock dummyBlock isIfAlive
  let (finalBlocks, areFinalBlocksAlive) = result
  addQuadrupleEdge currentBlockNumber newBlockNumber
  return (if not cond then currentBlockNumber:finalBlocks else finalBlocks, currentAlive || areFinalBlocksAlive)
transformStatement' (AST.IfElse _ expression ifStatement elseStatement) = do
  let ifBlock = AST.DummyBlock ifStatement
  let elseBlock = AST.DummyBlock elseStatement
  currentAlive <- view C.isAlive <$> getCurrentBlock
  expressionLocation <- transformExpression expression
  currentBlockNumber <- getCurrentBlockNumber
  assertLocationIsBool expressionLocation
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  (ifAlive, elseAlive) <- case expressionLocation of
    (Q.ConstBool x) -> return (currentAlive && x, currentAlive && (not x))
    _ -> return (currentAlive, currentAlive)
  (ifBlockNumber, (ifFinals, ifFinalsAlive)) <- transformBlock ifBlock ifAlive
  (elseBlockNumber, (elseFinals, elseFinalsAlive)) <- transformBlock elseBlock elseAlive
  addQuadrupleEdge currentBlockNumber ifBlockNumber
  addQuadrupleEdge currentBlockNumber elseBlockNumber 
  return (ifFinals ++ elseFinals, ifAlive || elseAlive)
transformStatement' (AST.While _ expression statement) = do
  let dummyBlock = AST.DummyBlock statement
  currentAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  addJumpPlaceholder
  leaveQuadrupleBlock
  _ <- newQuadrupleBlock currentAlive
  expressionLocation <- transformExpression expression
  conditionBlockNumber <- getCurrentBlockNumber
  assertLocationIsBool expressionLocation
  isWhileAlive <- case expressionLocation of
    (Q.ConstBool x) -> return $ currentAlive && x
    _ -> return currentAlive
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  (loopBlockNumber, (finalBlocks, areFinalBlocksAlive)) <- transformBlock dummyBlock currentAlive
  mapM_ (\b -> modifyBlock b $ over C.code (C.JumpPlaceholder:)) finalBlocks
  addQuadrupleEdge currentBlockNumber conditionBlockNumber
  addQuadrupleEdge conditionBlockNumber loopBlockNumber
  mapM_ (\s -> addQuadrupleEdge s conditionBlockNumber) finalBlocks
  return ([conditionBlockNumber], currentAlive)
transformStatement' (AST.Expression _ expression) = do
  _ <- transformExpression expression
  defaultStatementReturn

transformStatement :: StatementReturn -> AST.Statement -> C.FunctionTransformer StatementReturn
transformStatement ([], _) statement = do
  let position = getPosition statement
  when (position /= NoPosition) $ lift $ savePosition position
  transformStatement' statement
transformStatement (finalBlocks, anyAlive) statement = do
  let position = getPosition statement
  lift $ savePosition position
  assertNotInQuadrupleBlock
  newBlockNumber <- newQuadrupleBlock anyAlive
  mapM_ (\s -> addQuadrupleEdge s newBlockNumber) finalBlocks
  transformStatement' statement

transformBlock :: AST.Block -> Bool -> C.FunctionTransformer (Q.BlockNumber, StatementReturn)
transformBlock (AST.Block p statements) isAlive = do
  lift $ savePosition p
  newBlockNumber <- newQuadrupleBlock isAlive
  newScope
  (returnedBlocks, areFinalBlocksAlive) <- foldM transformStatement ([], isAlive) statements
  removeScope
  finalBlocks <- case returnedBlocks of
    [] -> (:[]) <$> getCurrentBlockNumber
    list -> return list
  gentlyLeaveQuadrupleBlock
  return (newBlockNumber, (finalBlocks, areFinalBlocksAlive))

transformFunctionBlock :: AST.Block -> C.FunctionTransformer ()
transformFunctionBlock (AST.Block p statements) = do
  lift $ savePosition p
  newScope
  x <- newQuadrupleBlock True
  modifyCurrentBlock $ set C.isAlive True
  arguments <- gets $ view C.arguments
  mapM_ (uncurry argumentInit) (zip [0..] arguments)
  (finalBlocks, _) <- foldM transformStatement ([], True) statements
  modify $ set C.finalBlocks finalBlocks
  gentlyLeaveQuadrupleBlock
  removeScope

transformFunction :: AST.Block -> C.FunctionTransformer Q.FunctionDefinition
transformFunction block = do
  transformFunctionBlock block
  returnType <- view C.returnType <$> get
  when (returnType == Void) addDummyReturnVoidBlock
  removeEdgesFromReturningBlocks
  addDummyJumps
  replacePreQuadruples 
  assertFinalBlocksHaveReturn
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

transformToQuadruples :: AST.Program -> Either (LatteError, Position) Q.Quadruples
transformToQuadruples program = case runGlobalTransformer program of
    Left error -> Left error
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
      when (length nextBlocks == 1) $ modifyBlock b $ over C.code (C.JumpPlaceholder:)
    ) blockNumbers

parsePreQuadruple :: C.PreQuadruple -> C.FunctionTransformer Q.Quadruple
parsePreQuadruple (C.Quadruple quadruple) = return quadruple
parsePreQuadruple _ = throwErrorFunction $ InternalCompilerError "Not able to parse prequadruple"

parseBlockContext :: C.BlockContext -> C.FunctionTransformer Q.Block
parseBlockContext blockContext = do
  let blockNumber = view C.blockNumber blockContext
  let finalVariables = view C.finalVariables blockContext
  let previousBlocks = view C.previousBlocks blockContext
  let nextBlocks = view C.nextBlocks blockContext
  let isAlive = view C.isAlive blockContext
  let preCode = view C.code blockContext
  code <- reverse <$> mapM parsePreQuadruple preCode
  let hasReturn = view C.hasReturn blockContext
  return $ Q.Block blockNumber finalVariables previousBlocks nextBlocks isAlive code hasReturn

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

replacePreQuadruple :: Q.BlockNumber -> C.PreQuadruple -> C.FunctionTransformer C.PreQuadruple
replacePreQuadruple blockNumber (C.PhiPlaceholder ident temporaryRegister) = do
  previousBlocksNumbers <- view C.previousBlocks <$> getBlock blockNumber
  maybeLocations <- map (Map.lookup ident . view C.finalVariables) <$> mapM getBlock previousBlocksNumbers
  unless (all isJust maybeLocations) $ throwErrorFunction $ InternalCompilerError "Not able to create phi function"
  let locations = map fromJust maybeLocations
  let phi = Q.Phi $ zip locations previousBlocksNumbers
  return $ C.Quadruple $ Q.QuadrupleOperation temporaryRegister phi 
replacePreQuadruple blockNumber C.JumpPlaceholder = do
  nextBlockNumbers <- view C.nextBlocks <$> getBlock blockNumber
  unless (length nextBlockNumbers == 1) $ throwErrorFunction $ InternalCompilerError "Not able to create jump operation"
  let target:[] = nextBlockNumbers
  let jump = Q.Jump target
  return $ C.Quadruple $ jump
replacePreQuadruple blockNumber (C.ConditionalJumpPlaceholder location) = do
  nextBlockNumbers <- view C.nextBlocks <$> getBlock blockNumber
  unless (length nextBlockNumbers == 2) $ throwErrorFunction $ InternalCompilerError "Not able to create conditional jump operation"
  let elseJump:ifJump:[] = nextBlockNumbers
  let conditionalJump = Q.ConditionalJump location ifJump elseJump
  return $ C.Quadruple $ conditionalJump
replacePreQuadruple _ preQuadruple = return preQuadruple

replacePreQuadruples :: C.FunctionTransformer ()
replacePreQuadruples = do
  blocks <- Map.elems . view C.blocks <$> get
  mapM_ (\block -> do
    let code = view C.code block
    let blockNumber = view C.blockNumber block
    newCode <- mapM (replacePreQuadruple blockNumber) code
    modifyBlock blockNumber $ set C.code newCode
    ) blocks
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

checkBlock :: C.BlockContext -> C.FunctionTransformer ()
checkBlock block = do
  let blockNumber = view C.blockNumber block
  finalBlocks <- traceShowId . view C.finalBlocks <$> get
  unless (blockNumber < 10000) $ throwErrorFunction $ InternalCompilerError "xyz"
  unless (length finalBlocks <10000) $ throwErrorFunction $ InternalCompilerError "xyz"

checkBlocks :: C.FunctionTransformer ()
checkBlocks = do
  blocks <- Map.elems . view C.blocks <$> get
  mapM_ (checkBlock . traceShowId) blocks