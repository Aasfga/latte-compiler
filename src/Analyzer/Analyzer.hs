module Analyzer.Analyzer where

import Analyzer.AnalyzerState
import AbstractSyntax.Definitions
import Control.Monad.State
import Control.Monad.Except

import Errors

getArgumentType :: Argument a -> Type
getArgumentType (Argument _ _type _) = _type

isStringOperation :: Operation -> Bool
isStringOperation Plus = True
isStringOperation _ = False

isIntOperation :: Operation -> Bool
isIntOperation Plus = True
isIntOperation Minus = True
isIntOperation Times = True
isIntOperation Div = True
isIntOperation Mod = True
isIntOperation _ = False

isBoolOperation :: Operation -> Bool
isBoolOperation And = True
isBoolOperation Or = True
isBoolOperation _ = False

analyzeOperation :: Type -> Operation -> Type -> Maybe Type
analyzeOperation Int op Int = 
  if isIntOperation op then Just Int else Nothing
analyzeOperation String op String = 
  if isStringOperation op then Just String else Nothing
analyzeOperation Bool op Bool = 
  if isBoolOperation op then Just Bool else Nothing
analyzeOperation _ _ _ = Nothing

isCorrectCompare :: Type -> Type -> Bool
isCorrectCompare Int Int = True
isCorrectCompare String String = True
isCorrectCompare Bool Bool = True
isCorrectCompare _ _ = False

isInt :: Type -> Bool
isInt Int = True
isInt _ = False

isBool :: Type -> Bool
isBool Bool = True
isBool _ = False

analyzeExpression :: Expression a -> AnalyzerState Type
analyzeExpression (Variable _ ident) = 
  getSymbolType ident
analyzeExpression (IntValue _ _) = 
  return Int
analyzeExpression (StringValue _ _) = 
  return String
analyzeExpression (BoolValue _ _) = 
  return Bool
analyzeExpression (Application _ ident expressions) = do
  found <- mapM analyzeExpression expressions
  identType <- getSymbolType ident
  case identType of
    Fun _type required -> do
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

analyzeDeclaration :: Declaration a -> Type -> AnalyzerState ()
analyzeDeclaration (NoInit _ ident) _type = addSymbol ident _type
analyzeDeclaration (Init _ ident expr) _type = do
  exprType <- analyzeExpression expr
  unless (exprType == _type) (throwError $ TypeMissmatchAssigment _type exprType)
  addSymbol ident _type 

calculateIfExpression :: Expression a -> Maybe Bool
calculateIfExpression (BoolValue _ value) = Just value
calculateIfExpression _ = Nothing

analyzeStatement :: Statement a -> AnalyzerState Bool
analyzeStatement (Empty _) = 
  return False
analyzeStatement (InnerBlock _ block) = 
  analyzeBlock block []
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
  return $ case calculateIfExpression expression of 
    Just True -> firstReturn
    Just False -> False
    Nothing -> False
analyzeStatement (IfElse _ expression firstBranch secondBranch) = do      
  _type <- analyzeExpression expression
  unless (isBool _type) (throwError $ TypeMissmatchIf _type)
  firstReturn <- analyzeStatement firstBranch
  secondReturn <- analyzeStatement secondBranch
  return $ case calculateIfExpression expression of
    Just True -> firstReturn
    Just False -> secondReturn
    Nothing -> firstReturn && secondReturn
analyzeStatement (While _ expression statement) = do
  _type <- analyzeExpression expression
  unless (isBool _type) (throwError $ TypeMissmatchIf _type)
  stmtReturn <- analyzeStatement statement
  return $ case calculateIfExpression expression of
    Just True -> stmtReturn
    Just False -> False
    Nothing -> False
analyzeStatement (Expression _ expression) = do
  _ <- analyzeExpression expression
  return False

analyzeBlock :: Block a -> [Argument a] -> AnalyzerState Bool
analyzeBlock (Block _ statements) arguments = do
  newScope
  mapM_ (\(Argument _ t i) -> addSymbol i t) arguments
  returnList <- mapM analyzeStatement statements
  removeScope
  return $ or returnList

analyzeFunction :: Function a -> AnalyzerState ()
analyzeFunction (Function _ _type ident arguments block) = do 
  setFunctionType _type
  isReturn <- analyzeBlock block arguments
  case _type of 
    Void -> return ()
    _ -> unless isReturn (throwError $ MissingReturn ident _type)

addFunctionsToScope :: Function a -> AnalyzerState ()
addFunctionsToScope (Function _ _type ident arguments _) = do
  let argumentTypes = map getArgumentType arguments
  addSymbol ident (Fun _type argumentTypes)

addPredefinedFunctions :: AnalyzerState ()
addPredefinedFunctions = do
  addSymbol "printInt" (Fun Void [Int])
  addSymbol "printString" (Fun Void [String])
  addSymbol "error" (Fun Void [])
  addSymbol "readInt" (Fun Int [])
  addSymbol "readString" (Fun String [])
 
analyzeProgram :: Program a -> AnalyzerState ()
analyzeProgram (Program _ functions) = do
  newScope
  addPredefinedFunctions
  mapM_ addFunctionsToScope functions
  mapM_ analyzeFunction functions
  removeScope

runAnalyzer :: Program a -> Either String String
runAnalyzer program = 
  let
    initialState = emptyAnalyzerState
  in case runStateT (analyzeProgram program) initialState of
    Left msg -> Left $ show msg
    Right _ -> Right "OK"

