module Analyzer.Analyzer where

import Analyzer.AnalyzerState
import IntermediateCode.Definitions.AbstractSyntaxTree as AST
import Types as T
import Control.Monad.State
import Control.Monad.Except
import Errors
import Analyzer.ExpressionCalculator

analyzeOperation :: Type -> Operation -> Type -> Maybe Type
analyzeOperation Int op Int = 
  if isIntOperation op then Just Int else Nothing
analyzeOperation String op String = 
  if isStringOperation op then Just String else Nothing
analyzeOperation Bool op Bool = 
  if isBoolOperation op then Just Bool else Nothing
analyzeOperation _ _ _ = Nothing

analyzeExpression :: Expression' a -> AnalyzerState Type
analyzeExpression (Variable _ ident) = 
  getSymbolType ident
analyzeExpression (Value _ value) = 
  case value of 
    IntValue x -> do
      unless (minInt <= x && x <= maxInt) (throwError $ IntegerOutOfBound x)
      return Int
    BoolValue _ -> return Bool
    StringValue _ -> return String
analyzeExpression (Application _ ident expressions) = do
  found <- mapM analyzeExpression expressions
  identType <- getSymbolType ident
  case identType of
    T.Function _type required -> do
      unless (found == required) (throwError $ TypeMissmatchApplication ident required found)
      return _type
    _ -> throwError $ FunctionNotFound ident
analyzeExpression (Neg _ expression) = do
  _type <- analyzeExpression expression
  unless (isInt _type) (throwError $ TypeMissmatchUnaryOperator _type "~")
  return Int
analyzeExpression (Not _ expression) = do
  _type <- analyzeExpression expression
  unless (isBool _type) (throwError $ TypeMissmatchUnaryOperator _type "!")
  return Bool
analyzeExpression (Operation _ firstExpr op secondExpr) = do
  firstType <- analyzeExpression firstExpr
  secondType <- analyzeExpression secondExpr
  case analyzeOperation firstType op secondType of 
    Just _type -> return _type
    Nothing -> throwError $ TypeMissmatchBinaryOperator firstType secondType op
analyzeExpression (Compare _ firstExpr _ secondExpr) = do
  firstType <- analyzeExpression firstExpr
  secondType <- analyzeExpression secondExpr
  unless (isCorrectCompare firstType secondType) (throwError $ TypeMissmatchCompare firstType secondType)
  return Bool

analyzeDeclaration :: Declaration' a -> Type -> AnalyzerState ()
analyzeDeclaration (NoInit _ ident) _type = addSymbol ident _type
analyzeDeclaration (Init _ ident expr) _type = do
  exprType <- analyzeExpression expr
  unless (exprType == _type) (throwError $ TypeMissmatchAssigment _type exprType)
  addSymbol ident _type 

analyzeStatement :: Statement' a -> AnalyzerState Bool
analyzeStatement (Empty _) = 
  return False
analyzeStatement (InnerBlock _ block) = do
  newScope 
  blockReturn <- analyzeBlock block []
  removeScope 
  return blockReturn
analyzeStatement (Declaration _ _type declarations) = do
  mapM_ (`analyzeDeclaration` _type) declarations
  return False
analyzeStatement (Assigment _ ident expression) = do
  exprType <- analyzeExpression expression
  _type <- getSymbolType ident
  unless (_type == exprType) (throwError $ TypeMissmatchAssigment _type exprType)
  return False
analyzeStatement (Increment _ ident) = do
  _type <- getSymbolType ident
  unless (isInt _type) (throwError $ TypeMissmatchUnaryOperator _type "++")
  return False
analyzeStatement (Decrement _ ident) = do
  _type <- getSymbolType ident
  unless (isInt _type) (throwError $ TypeMissmatchUnaryOperator _type "--")
  return False
analyzeStatement (Return _ expr) = do
  _type <- analyzeExpression expr
  checkReturnType _type
  return True
analyzeStatement (VoidReturn _) = do
  checkReturnType Void
  return True
analyzeStatement (If _ expression firstBranch) = do
  _type <- analyzeExpression expression 
  unless (_type == Bool) (throwError $ TypeMissmatchIf _type)
  firstReturn <- analyzeStatement firstBranch
  return $ case calculateBoolExpression expression of 
    Just True -> firstReturn
    Just False -> False
    Nothing -> False
analyzeStatement (IfElse _ expression firstBranch secondBranch) = do      
  _type <- analyzeExpression expression
  unless (isBool _type) (throwError $ TypeMissmatchIf _type)
  firstReturn <- analyzeStatement firstBranch
  secondReturn <- analyzeStatement secondBranch
  return $ case calculateBoolExpression expression of
    Just True -> firstReturn
    Just False -> secondReturn
    Nothing -> firstReturn && secondReturn
analyzeStatement (While _ expression statement) = do
  _type <- analyzeExpression expression
  unless (isBool _type) (throwError $ TypeMissmatchIf _type)
  stmtReturn <- analyzeStatement statement
  return $ case calculateBoolExpression expression of
    -- TODO: Check!
    Just True -> True
    Just False -> False
    Nothing -> False
analyzeStatement (Expression _ expression) = do
  _ <- analyzeExpression expression
  return False

analyzeBlock :: Block' a -> [Argument] -> AnalyzerState Bool
analyzeBlock (Block _ statements) arguments = do
  newScope
  mapM_ (\(Argument t i) -> addSymbol i t) arguments
  returnList <- mapM analyzeStatement statements
  removeScope
  return $ or returnList

analyzeFunction :: Function' a -> AnalyzerState ()
analyzeFunction (AST.Function _ _type ident arguments block) = do 
  setFunctionType _type
  isReturn <- analyzeBlock block arguments
  case _type of 
    Void -> return ()
    _ -> unless isReturn (throwError $ MissingReturn ident _type)

addFunctionsToScope :: Function' a -> AnalyzerState ()
addFunctionsToScope (AST.Function _ _type ident arguments _) = do
  let argumentTypes = map getArgumentType arguments
  addSymbol ident (T.Function _type argumentTypes)

addLibraryFunctions :: AnalyzerState ()
addLibraryFunctions = mapM_ (uncurry addSymbol) libraryFunctionsOld
 
checkIfMainExists :: AnalyzerState ()
checkIfMainExists = do
  mainType <- getSymbolType "main"
  unless (mainType == T.Function Int []) (throwError $ SymbolNotFound "main")

analyzeProgram :: Program' a -> AnalyzerState ()
analyzeProgram (Program _ functions) = do
  newScope
  addLibraryFunctions
  mapM_ addFunctionsToScope functions
  checkIfMainExists
  mapM_ analyzeFunction functions
  removeScope

runAnalyzer :: Program' a -> Either LatteError ()
runAnalyzer program = 
  let
    initialState = emptyAnalyzerState
  in case runStateT (analyzeProgram program) initialState of
    Left analyzerError -> Left $ analyzerError
    Right _ -> Right ()

