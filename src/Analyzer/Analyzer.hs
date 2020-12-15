module Analyzer.Analyzer where

import Analyzer.AnalyzerState
import AbstractSyntax.Definitions
import Control.Monad.State
import Control.Monad.Except

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
  if isIntOperation op then Just String else Nothing
analyzeOperation Bool op Bool = 
  if isIntOperation op then Just Bool else Nothing
analyzeOperation _ _ _ = Nothing

isCorrectCompare :: Type -> Type -> Bool
isCorrectCompare Int Int = True
isCorrectCompare String String = True
isCorrectCompare Bool Bool = True
isCorrectCompare _ _ = False

analyzeExpression :: Expression a -> AnalyzerState Type
analyzeExpression (Variable _ ident) = getSymbolType ident
analyzeExpression (IntValue _ _) = return Int
analyzeExpression (StringValue _ _) = return String
analyzeExpression (BoolValue _ _) = return Bool
analyzeExpression (Application _ ident expressions) = do
  expTypes <- mapM analyzeExpression expressions
  _type <- getSymbolType ident
  case _type of
    Fun retType argTypes -> 
      if expTypes == argTypes then 
        return retType
      else 
        throwError "Wrong type"
    _ -> throwError "Wrong type"
analyzeExpression (Neg _ expression) = do
  _type <- analyzeExpression expression
  case _type of 
    Int -> return Int
    _ -> throwError "Wrong type"
analyzeExpression (Not _ expression) = do
  _type <- analyzeExpression expression
  case _type of 
    Bool -> return Bool
    _ -> throwError "Wrong type"
analyzeExpression (Operation _ firstExpr op secondExpr) = do
  firstType <- analyzeExpression firstExpr
  secondType <- analyzeExpression secondExpr
  case analyzeOperation firstType op secondType of 
    Just _type -> return _type
    Nothing -> throwError "Wrong type"
analyzeExpression (Compare _ firstExpr _ secondExpr) = do
  firstType <- analyzeExpression firstExpr
  secondType <- analyzeExpression secondExpr
  if isCorrectCompare firstType secondType then 
    return Bool
  else
    throwError "Wrong type"

analyzeDeclaration :: Declaration a -> Type -> AnalyzerState ()
analyzeDeclaration (NoInit _ ident) _type = addSymbol ident _type
analyzeDeclaration (Init _ ident expr) _type = do
  exprType <- analyzeExpression expr
  if exprType == _type then do
    addSymbol ident _type 
    return ()
  else
    throwError "Wrong type"

analyzeStatement :: Statement a -> AnalyzerState ()
analyzeStatement (Empty _) = 
  return ()
analyzeStatement (InnerBlock _ block) = 
  analyzeBlock block []
analyzeStatement (Declaration _ _type declarations) = 
  mapM_ (`analyzeDeclaration` _type) declarations
analyzeStatement (Assigment _ ident expression) = do
  exprType <- analyzeExpression expression
  varType <- getSymbolType ident
  unless (varType == exprType) (throwError "Wrong type")
analyzeStatement (Increment _ ident) = do
  _type <- getSymbolType ident
  case _type of 
    Int -> return ()
    _ -> throwError "Trying to increment ..."
analyzeStatement (Decrement _ ident) = do
  _type <- getSymbolType ident
  case _type of 
    Int -> return ()
    _ -> throwError "Trying to increment ..."
analyzeStatement (Return _ expr) = do
  _type <- analyzeExpression expr
  checkReturnType _type
analyzeStatement (VoidReturn _) =
  checkReturnType Void
analyzeStatement (If _ expression firstBranch) = do
  _type <- analyzeExpression expression 
  unless (_type == Bool) (throwError "Wrong type in if")
  analyzeStatement firstBranch
analyzeStatement (IfElse _ expression firstBranch secondBranch) = do      
  _type <- analyzeExpression expression
  case _type of 
    Bool -> do 
      analyzeStatement firstBranch
      analyzeStatement secondBranch
    _ -> throwError "Wrong type in if"
analyzeStatement (While _ expression statement) = do
  _type <- analyzeExpression expression
  case _type of
    Bool -> analyzeStatement statement
    _ -> throwError "Wrong type in if"
analyzeStatement (Expression _ expression) = do
  _ <- analyzeExpression expression
  return ()

analyzeBlock :: Block a -> [Argument a] -> AnalyzerState ()
analyzeBlock (Block _ statements) arguments = do
  newScope
  mapM_ (\(Argument _ t i) -> addSymbol i t) arguments
  mapM_ analyzeStatement statements
  removeScope

analyzeFunction :: Function a -> AnalyzerState ()
analyzeFunction (Function _ _type _ arguments block) = do 
  setFunctionType _type
  analyzeBlock block arguments

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
    Left msg -> Left msg
    Right _ -> Right "OK"
