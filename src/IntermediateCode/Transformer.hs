
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
import qualified IntermediateCode.Transformer.TransformerContext as C
import Lens.Micro.Platform
import Debug.Trace
import IntermediateCode.Transformer.Utilities
import IntermediateCode.Transformer.QuadrupleUtilities

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
transformBinaryOperation _type op = (\_ _ -> throwError $ TypeMissmatchBinaryOperator _type _type op)

transformExpression :: AST.Expression -> C.FunctionTransformer Q.QuadrupleLocation 
transformExpression (AST.Variable p ident) = getLocation ident
transformExpression (AST.Value p (IntValue x)) = do 
  unless (minInt <= x && x <= maxInt) $ throwError $ IntegerOutOfBound x
  return $ Q.ConstInt x
transformExpression (AST.Value _ (BoolValue x)) = return $ Q.ConstBool x
transformExpression (AST.Value _ (StringValue x)) = return $ Q.ConstString x
transformExpression (AST.Application p ident expressions) = do
  locations <- mapM transformExpression expressions
  callFunction ident locations
transformExpression (AST.Neg p expression) = do
  location <- transformExpression expression
  integerSub (Q.ConstInt 0) location
transformExpression (AST.Not p expression) = do
  location <- transformExpression expression
  boolNot location 
transformExpression (AST.Operation p firstExpression op secondExpression) = do
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  case (Q.getQuadrupleLocationType firstLocation, Q.getQuadrupleLocationType secondLocation) of
    (Int, Int) -> transformBinaryOperation Int op firstLocation secondLocation
    (Bool, Bool) -> transformBinaryOperation Bool op firstLocation secondLocation
    (String, String) -> transformBinaryOperation String op firstLocation secondLocation
    (firstType, secondType) -> throwError $ TypeMissmatchBinaryOperator firstType secondType op
transformExpression (AST.Compare p firstExpression op secondExpression) = do
  firstLocation <- transformExpression firstExpression
  secondLocation <- transformExpression secondExpression
  case (Q.getQuadrupleLocationType firstLocation, Q.getQuadrupleLocationType secondLocation) of
    (Int, Int) -> integerCompare firstLocation op secondLocation
    (Bool, Bool) -> boolCompare firstLocation op secondLocation
    (String, String) -> stringCompare firstLocation op secondLocation
    (firstType, secondType) -> throwError $ TypeMissmatchCompare firstType secondType

transformDeclaration :: Type -> AST.Declaration -> C.FunctionTransformer ()
transformDeclaration _type (AST.NoInit p ident) = do
  defaultValue <- getDefaultValue _type
  newVariable _type ident $ Q.ConstValue defaultValue
transformDeclaration _type (AST.Init p ident expression) = do
  expressionLocation <- transformExpression expression
  let expressionType = Q.getQuadrupleLocationType expressionLocation
  assertLocationType expressionLocation _type
  newVariable _type ident expressionLocation

transformStatement' :: AST.Statement -> C.FunctionTransformer [(Q.BlockNumber, Bool)]
transformStatement' (AST.Empty _) = return []
transformStatement' (AST.InnerBlock p block) = do
  isPreviousBlockAlive <- view C.isAlive <$> getCurrentBlock
  previousBlockNumber <- getCurrentBlockNumber
  addJumpPlaceholder
  leaveQuadrupleBlock
  (newBlockNumber, finalBlocks) <- transformBlock block isPreviousBlockAlive
  addQuadrupleEdge previousBlockNumber newBlockNumber
  return $ makeFinalBlocks (newBlockNumber, isPreviousBlockAlive) finalBlocks
transformStatement' (AST.Declaration _ _type declarations) = do
  mapM_ (transformDeclaration _type) declarations
  return []
transformStatement' (AST.Assigment p ident expression) = do
  expressionLocation <- transformExpression expression
  let expressionType = Q.getQuadrupleLocationType expressionLocation
  assertVariableType ident expressionType
  setLocation ident expressionLocation
  return []
transformStatement' (AST.Increment p ident) = do
  assertVariableType ident Int
  location <- getLocation ident
  newLocation <- integerAdd location (Q.ConstInt 1)
  setLocation ident newLocation
  return []
transformStatement' (AST.Decrement p ident) = do
  assertVariableType ident Int
  location <- getLocation ident
  newLocation <- integerSub location (Q.ConstInt 1)
  setLocation ident newLocation
  return []
transformStatement' (AST.Return p expression) = do
  -- TODO what if return is not last
  expressionLocation <- transformExpression expression
  returnExpression expressionLocation
  return []
transformStatement' (AST.VoidReturn p) = do
  -- TODO what if return is not last
  returnVoid
  modifyCurrentBlock $ set C.hasReturn True
  return []
transformStatement' (AST.If p expression statement) = do
  isPreviousBlockAlive <- view C.isAlive <$> getCurrentBlock
  let dummyBlock = AST.DummyBlock statement
  previousBlockNumber <- getCurrentBlockNumber
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  isAlive <- case expressionLocation of 
    (Q.ConstBool x) -> return $ isPreviousBlockAlive && x
    _ -> return isPreviousBlockAlive
  (newBlockNumber, finalBlocks) <- transformBlock dummyBlock isAlive
  addQuadrupleEdge previousBlockNumber newBlockNumber
  return $ (previousBlockNumber, isPreviousBlockAlive):(makeFinalBlocks (newBlockNumber, isAlive) finalBlocks)
transformStatement' (AST.IfElse p expression first second) = do
  isPreviousBlockAlive <- view C.isAlive <$> getCurrentBlock
  let firstDummyBlock = AST.DummyBlock first
  let secondDummyBlock = AST.DummyBlock second
  previousBlockNumber <- getCurrentBlockNumber
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  (isFirstAlive, isSecondAlive) <- case expressionLocation of
    (Q.ConstBool x) -> return (isPreviousBlockAlive && x, isPreviousBlockAlive && not x)
    _ -> return (isPreviousBlockAlive, isPreviousBlockAlive)
  (firstNumber, firstFinalBlocks) <- transformBlock firstDummyBlock isFirstAlive
  (secondNumber, secondFinalBlocks) <- transformBlock secondDummyBlock isSecondAlive
  addQuadrupleEdge previousBlockNumber firstNumber
  addQuadrupleEdge previousBlockNumber secondNumber
  return $ (makeFinalBlocks (firstNumber, isFirstAlive) firstFinalBlocks) ++ 
           (makeFinalBlocks (secondNumber, isSecondAlive) secondFinalBlocks)
transformStatement' (AST.While p expression statement) = do
  isPreviousBlockAlive <- view C.isAlive <$> getCurrentBlock
  let dummyBlock = AST.DummyBlock statement
  previousBlockNumber <- getCurrentBlockNumber
  addJumpPlaceholder
  leaveQuadrupleBlock
  conditionBlockNumber <- newQuadrupleBlock isPreviousBlockAlive
  expressionLocation <- transformExpression expression
  assertLocationIsBool expressionLocation
  isAlive <- case expressionLocation of
    (Q.ConstBool x) -> return $ isPreviousBlockAlive && x
    _ -> return isPreviousBlockAlive
  addConditionalJumpPlaceholder expressionLocation
  leaveQuadrupleBlock
  (loopBlockNumber, blocks) <- transformBlock dummyBlock isAlive
  addQuadrupleEdge previousBlockNumber conditionBlockNumber
  addQuadrupleEdge conditionBlockNumber loopBlockNumber
  mapM_ (\b -> addQuadrupleEdge (fst b) conditionBlockNumber) blocks
  return [(conditionBlockNumber, isAlive)]
transformStatement' (AST.Expression p expression) = do
  _ <- transformExpression expression
  return []

transformStatement :: [(Q.BlockNumber, Bool)] -> AST.Statement -> C.FunctionTransformer [(Q.BlockNumber, Bool)]
transformStatement [] statement = transformStatement' statement
transformStatement finalBlocks statement = do
  assertNotInQuadrupleBlock
  newBlockNumber <- newQuadrupleBlock $ any snd finalBlocks
  mapM_ (\s -> addQuadrupleEdge (fst s) newBlockNumber) finalBlocks
  transformStatement' statement

transformBlock :: AST.Block -> Bool -> C.FunctionTransformer (Q.BlockNumber, [(Q.BlockNumber, Bool)])
transformBlock (AST.Block p statements) isAlive = do
  newBlockNumber <- newQuadrupleBlock isAlive
  newScope
  finalBlocks <- foldM transformStatement [] statements
  removeScope
  leaveQuadrupleBlock
  return (newBlockNumber, makeFinalBlocks (newBlockNumber, isAlive) finalBlocks)

transformFunction :: AST.Function -> C.FunctionTransformer ()
transformFunction (AST.Function p returnType _ arguments block) = do
  let (AST.Block _ statements) = block
  newScope
  _ <- newQuadrupleBlock True
  modifyCurrentBlock $ set C.isAlive True
  arguments <- gets $ view C.arguments
  mapM_ (uncurry argumentInit) (zip [0..] arguments)
  finalBlocks <- foldM transformStatement [] statements
  removeScope
  assertFinalBlocksHaveReturn (trace "finalBlocks" finalBlocks)

transformFunctionToQuadruples :: AST.Function -> C.GlobalTransformer ()
transformFunctionToQuadruples function = do
  result <- runFunctionTransformer transformFunction function
  return ()

transformProgram :: AST.Program -> C.GlobalTransformer ()
transformProgram program@(AST.Program p functions) = do
  defineGlobalSymbols program
  mapM_ transformFunctionToQuadruples functions
  assertMainExists

transformToQuadruples :: AST.Program -> Either LatteError Q.QuadruplesCode
transformToQuadruples program = case runGlobalTransformer transformProgram program of
    Left error -> Left error
    Right (_, code) -> Right code
-- 
-- Transformers
-- 
runGlobalTransformer :: (AST.Program -> C.GlobalTransformer ()) -> AST.Program -> Either LatteError ((), Q.QuadruplesCode)
runGlobalTransformer transformer program = let
    initialState = C.emptyGlobalContext 
  in
    runStateT (transformer program) initialState

runFunctionTransformer :: (AST.Function -> C.FunctionTransformer ()) -> AST.Function -> C.GlobalTransformer ((), C.FunctionContext)
runFunctionTransformer transformer function = let
    (AST.Function _ returnType ident arguments _) = function
    initialState = C.emptyFunctionContext returnType ident arguments 
  in
    runStateT (transformer function) initialState