
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
transformBinaryOperation _type op = (\_ _ -> throwLatteError $ TypeMissmatchBinaryOperator _type _type op)

transformExpression :: AST.Expression -> C.FunctionTransformer Q.QuadrupleLocation 
transformExpression (AST.Variable p ident) = getLocation ident
transformExpression (AST.Value p (AST.IntegerValue x)) = do 
  -- unless (minBound :: Int) <= x && x <= maxInt) $ throwError $ IntegerOutOfBound x
  return $ Q.ConstInt $ fromIntegral x
transformExpression (AST.Value _ (AST.BoolValue x)) = return $ Q.ConstBool x
transformExpression (AST.Value _ (AST.StringValue x)) = return $ Q.ConstString x
transformExpression (AST.Application p ident expressions) = do
  savePosition p
  locations <- mapM transformExpression expressions
  callFunction ident locations
transformExpression (AST.Neg p expression) = do
  savePosition p
  location <- transformExpression expression
  integerSub (Q.ConstInt 0) location
transformExpression (AST.Not p expression) = do
  savePosition p
  location <- transformExpression expression
  boolNot location 
transformExpression (AST.Operation p firstExpression op secondExpression) = do
  savePosition p
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  case (getType firstLocation, getType secondLocation) of
    (Int, Int) -> transformBinaryOperation Int op firstLocation secondLocation
    (Bool, Bool) -> transformBinaryOperation Bool op firstLocation secondLocation
    (String, String) -> transformBinaryOperation String op firstLocation secondLocation
    (firstType, secondType) -> throwLatteError $ TypeMissmatchBinaryOperator firstType secondType op
transformExpression (AST.Compare p firstExpression op secondExpression) = do
  savePosition p
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  case (getType firstLocation, getType secondLocation) of
    (Int, Int) -> integerCompare firstLocation op secondLocation
    (Bool, Bool) -> boolCompare firstLocation op secondLocation
    (String, String) -> stringCompare firstLocation op secondLocation
    (firstType, secondType) -> throwLatteError $ TypeMissmatchCompare firstType secondType

transformDeclaration :: Type -> AST.Declaration -> C.FunctionTransformer ()
transformDeclaration _type (AST.NoInit p ident) = do
  savePosition p
  constValue <- getDefaultConstValue _type
  newVariable _type ident constValue
transformDeclaration _type (AST.Init p ident expression) = do
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
transformStatement' (AST.InnerBlock p block) = do
  savePosition p
  currentAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  addJumpPlaceholder
  leaveQuadrupleBlock 
  (newBlockNumber, result) <- transformBlock block currentAlive
  let (finalBlocks, areFinalBlocksAlive) = result
  addQuadrupleEdge currentBlockNumber newBlockNumber
  return result
transformStatement' (AST.Declaration p _type declarations) = do
  savePosition p
  mapM_ (transformDeclaration _type) declarations
  defaultStatementReturn
transformStatement' (AST.Assigment p ident expression) = do
  savePosition p
  expressionLocation <- transformExpression expression
  let expressionType = getType expressionLocation
  assertVariableType ident expressionType
  setLocation ident expressionLocation
  defaultStatementReturn
transformStatement' (AST.Increment p ident) = do
  savePosition p
  assertVariableType ident Int
  location <- getLocation ident
  newLocation <- integerAdd location (Q.ConstInt 1)
  setLocation ident newLocation
  defaultStatementReturn
transformStatement' (AST.Decrement p ident) = do
  savePosition p
  assertVariableType ident Int
  location <- getLocation ident
  newLocation <- integerSub location (Q.ConstInt 1)
  setLocation ident newLocation
  defaultStatementReturn
transformStatement' (AST.Return p expression) = do
  savePosition p
  expressionLocation <- transformExpression expression
  returnExpression expressionLocation
  modifyCurrentBlock $ set C.hasReturn True
  defaultStatementReturn
  -- leaveQuadrupleBlock
  -- return ([currentBlockNumber], False)
transformStatement' (AST.VoidReturn p) = do
  savePosition p
  currentBlockNumber <- getCurrentBlockNumber
  returnVoid
  modifyCurrentBlock $ set C.hasReturn True
  defaultStatementReturn
  -- leaveQuadrupleBlock
  -- return ([currentBlockNumber], False)
transformStatement' (AST.If p expression statement) = do
  savePosition p
  let dummyBlock = AST.DummyBlock statement
  currentAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  (isIfAlive, cond) <- case expressionLocation of 
    (Q.ConstBool x) -> return $ (currentAlive && x, x)
    _ -> return (currentAlive, False)
  (newBlockNumber, result) <- transformBlock dummyBlock isIfAlive
  let (finalBlocks, areFinalBlocksAlive) = result
  addQuadrupleEdge currentBlockNumber newBlockNumber
  return (if not cond then currentBlockNumber:finalBlocks else finalBlocks, currentAlive || areFinalBlocksAlive)
transformStatement' (AST.IfElse p expression ifStatement elseStatement) = do
  savePosition p
  let ifBlock = AST.DummyBlock ifStatement
  let elseBlock = AST.DummyBlock elseStatement
  currentAlive <- view C.isAlive <$> getCurrentBlock
  currentBlockNumber <- getCurrentBlockNumber
  expressionLocation <- transformExpression expression
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
transformStatement' (AST.While p expression statement) = do
  savePosition p
  let dummyBlock = AST.DummyBlock statement
  currentBlockNumber <- getCurrentBlockNumber
  currentAlive <- view C.isAlive <$> getCurrentBlock
  addJumpPlaceholder
  leaveQuadrupleBlock
  conditionBlockNumber <- newQuadrupleBlock currentAlive
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  isWhileAlive <- case expressionLocation of
    (Q.ConstBool x) -> return $ currentAlive && x
    _ -> return currentAlive
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  (loopBlockNumber, (finalBlocks, areFinalBlocksAlive)) <- transformBlock dummyBlock currentAlive
  addQuadrupleEdge currentBlockNumber conditionBlockNumber
  addQuadrupleEdge conditionBlockNumber loopBlockNumber
  mapM_ (\s -> addQuadrupleEdge s conditionBlockNumber) finalBlocks
  return ([conditionBlockNumber], currentAlive)
transformStatement' (AST.Expression p expression) = do
  _ <- transformExpression expression
  defaultStatementReturn

transformStatement :: StatementReturn -> AST.Statement -> C.FunctionTransformer StatementReturn
transformStatement ([], _) statement = transformStatement' statement
transformStatement (finalBlocks, anyAlive) statement = do
  assertNotInQuadrupleBlock
  newBlockNumber <- newQuadrupleBlock anyAlive
  mapM_ (\s -> addQuadrupleEdge s newBlockNumber) finalBlocks
  transformStatement' statement

transformBlock :: AST.Block -> Bool -> C.FunctionTransformer (Q.BlockNumber, StatementReturn)
transformBlock (AST.Block p statements) isAlive = do
  savePosition p
  newBlockNumber <- newQuadrupleBlock isAlive
  newScope
  (returnedBlocks, areFinalBlocksAlive) <- foldM transformStatement ([], isAlive) statements
  removeScope
  finalBlocks <- case returnedBlocks of
    [] -> (:[]) <$> getCurrentBlockNumber
    list -> return list
  gentlyLeaveQuadrupleBlock
  return (newBlockNumber, (finalBlocks, areFinalBlocksAlive))

transformFunction :: AST.Block -> C.FunctionTransformer ()
transformFunction (AST.Block _ statements) = do
  newScope
  x <- newQuadrupleBlock True
  modifyCurrentBlock $ set C.isAlive True
  arguments <- gets $ view C.arguments
  mapM_ (uncurry argumentInit) (zip [0..] arguments)
  (finalBlocks, _) <- foldM transformStatement ([], True) statements
  modify $ set C.finalBlocks finalBlocks
  gentlyLeaveQuadrupleBlock
  removeScope
  assertFinalBlocksHaveReturn
  assertFinalBlocksHaveReturn2

transformGlobalSymbolToQuadruples :: AST.GlobalSymbol -> C.GlobalTransformer ()
transformGlobalSymbolToQuadruples (AST.Function _ returnType functionName arguments block) = do
  result <- runFunctionTransformer returnType functionName arguments block
  return ()

transformProgram :: AST.Program -> C.GlobalTransformer ()
transformProgram (AST.Program p globalSymbols) = do
  defineGlobalSymbols globalSymbols
  mapM_ transformGlobalSymbolToQuadruples globalSymbols
  assertMainExists

transformToQuadruples :: AST.Program -> Either (LatteError, Position) Q.QuadruplesCode
transformToQuadruples program = case runGlobalTransformer transformProgram program of
    Left error -> Left error
    Right (_, code) -> Right code
-- 
-- Transformers
-- 
runGlobalTransformer :: (AST.Program -> C.GlobalTransformer ()) -> AST.Program -> Either (LatteError, Position) ((), Q.QuadruplesCode)
runGlobalTransformer transformer program = let
    initialState = C.emptyGlobalContext 
  in
    runStateT (transformer program) initialState

runFunctionTransformer :: Type -> Ident -> [Argument] -> AST.Block -> C.GlobalTransformer ((), C.FunctionContext)
runFunctionTransformer returnType functionIdent arguments block = let
    initialState = C.emptyFunctionContext returnType functionIdent arguments 
  in
    runStateT (transformFunction block) initialState