module Analyzer.AnalyzerState where

import AbstractSyntax.Definitions
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except

type AnalyzeError = String
type SymbolInfo = Type
type SymbolTable = Map.Map Ident [SymbolInfo]
type Scope = [Ident]
type AccessInfo = Bool
type AnalyzerState = StateT (SymbolTable, [Scope], [AccessInfo], Type) (Either AnalyzeError)

emptyAnalyzerState :: (SymbolTable, [Scope], [AccessInfo], Type)
emptyAnalyzerState = (Map.empty, [], [], Int)

modifyScopeStack :: ([Scope] -> [Scope]) -> AnalyzerState () 
modifyScopeStack f = do
  (x, scopes, z, t) <- get
  put (x, f scopes, z, t)

newScope :: AnalyzerState ()
newScope = modifyScopeStack ([]:)

removeScope :: AnalyzerState ()
removeScope = do
  (x, scopes, z, t) <- get
  case scopes of 
    [] -> throwError "No scope defined"
    (_:rest) -> put (x, rest, z, t)

currentScope :: AnalyzerState Scope
currentScope = do
  (_, scopes, _, _) <- get
  case scopes of 
    [] -> throwError "No scope defined"
    (scope:_) -> return scope

modifyScope :: (Scope -> Scope) -> AnalyzerState ()
modifyScope f = do
  (x, scopes, z, t) <- get
  case scopes of 
    [] -> throwError "No scope defined"
    (y:ys) -> put (x, f y:ys, z, t)

modifySymbolTable :: (SymbolTable -> SymbolTable) -> AnalyzerState ()
modifySymbolTable f = do
  (table, y, z, t) <- get
  put (f table, y, z, t)

getSymbolTable :: AnalyzerState SymbolTable
getSymbolTable = do
  (table, _, _ , _) <- get 
  return table

setFunctionType :: Type -> AnalyzerState ()
setFunctionType _type= do
  (x, y, z, _) <- get
  put (x, y, z, _type)

getFunctionType :: AnalyzerState Type
getFunctionType = do
  (_, _, _, _type) <- get
  return _type

canDefineSymbol :: Ident -> AnalyzerState Bool
canDefineSymbol ident = notElem ident <$> currentScope

checkReturnType :: Type -> AnalyzerState ()
checkReturnType _type = do 
  funType <- getFunctionType
  unless (_type == funType) (throwError "Wrong ret type")

addSymbol :: Ident -> Type -> AnalyzerState ()
addSymbol ident _type = do
  canDefine <- canDefineSymbol ident
  if not canDefine then
    throwError "Symbol defined"
  else do
    modifyScope (ident:)
    modifySymbolTable $ Map.insertWith (++) ident [_type]

getSymbolType :: Ident -> AnalyzerState Type
getSymbolType ident = do
  table <- getSymbolTable
  case Map.lookup ident table of 
    Nothing -> throwError "Variable not defined"
    Just [] -> throwError "Variable not defined"
    Just (_type:_) -> return _type
    