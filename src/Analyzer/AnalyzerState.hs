module Analyzer.AnalyzerState where

import AbstractSyntax.Definitions
import qualified Data.Map as Map
import Control.Monad.State
-- import qualified Control.Monad.Except as Except
import Control.Monad.Except
import Errors

type SymbolInfo = Type
type SymbolTable = Map.Map Ident [SymbolInfo]
type Scope = [Ident]
type AccessInfo = Bool
type AnalyzerState = StateT (SymbolTable, [Scope], [AccessInfo], Type) (Either AnalyzerError)

emptyAnalyzerState :: (SymbolTable, [Scope], [AccessInfo], Type)
emptyAnalyzerState = (Map.empty, [], [], Int)

modifyScopeStack :: ([Scope] -> [Scope]) -> AnalyzerState () 
modifyScopeStack f = do
  (x, scopes, z, t) <- get
  put (x, f scopes, z, t)

newScope :: AnalyzerState ()
newScope = modifyScopeStack ([]:)

popSymbol :: Ident -> AnalyzerState ()
popSymbol ident = do
  table <- getSymbolTable
  let symbols = Map.lookup ident table
  case symbols of
    Just (_:xs) -> modifySymbolTable $ Map.insert ident xs
    _ -> throwError $ InternalError ("No symbol for ident " ++ ident)

removeScope :: AnalyzerState ()
removeScope = do
  (x, scopes, z, t) <- get
  case scopes of 
    [] -> throwError $ InternalError "No scope defined"
    (scope:rest) -> do
      put (x, rest, z, t)
      mapM_ popSymbol scope

currentScope :: AnalyzerState Scope
currentScope = do
  (_, scopes, _, _) <- get
  case scopes of 
    [] -> throwError $ InternalError "No scope defined"
    (scope:_) -> return scope

modifyScope :: (Scope -> Scope) -> AnalyzerState ()
modifyScope f = do
  (x, scopes, z, t) <- get
  case scopes of 
    [] -> throwError $ InternalError "No scope defined"
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
checkReturnType found = do 
  required <- getFunctionType
  unless (found == required) (throwError $ TypeMissmatchReturn "fun" required found)

addSymbol :: Ident -> Type -> AnalyzerState ()
addSymbol ident _type = do
  canDefine <- canDefineSymbol ident
  unless canDefine (throwError $ SymbolInScope ident _type)
  modifyScope (ident:)
  modifySymbolTable $ Map.insertWith (++) ident [_type]

getSymbolType :: Ident -> AnalyzerState Type
getSymbolType ident = do
  table <- getSymbolTable
  case Map.lookup ident table of 
    Nothing -> throwError $ SymbolNotFound ident
    Just [] -> throwError $ SymbolNotFound ident
    Just (_type:_) -> return _type