module Analyzer.Analyzer where
  

-- import qualified Data.Map as Map
-- import Parser.AbsLatte
--     ( Type, Program(..), Ident(..), Argument(..), Function(..) )
-- import Control.Monad.State
-- import Control.Monad.Except



-- type SymbolInfo = Type ()
-- type SymbolTable = Map.Map String [SymbolInfo]
-- type Stack a = [a]
-- type Scope = [String]
-- type ScopeStack = Stack Scope
-- type AccessStack = Stack Bool
-- type AnalyzerState = StateT (SymbolTable, ScopeStack, AccessStack) (Either String)

-- emptyAnalyzerState :: (SymbolTable, ScopeStack, AccessStack)
-- emptyAnalyzerState = (Map.empty, [], [])

-- modifyST :: (SymbolTable -> SymbolTable) -> AnalyzerState ()
-- modifyST f = do
--   (varMap, declarationStack, accessStack) <- get
--   put (f varMap, declarationStack, accessStack)

-- getST :: AnalyzerState SymbolTable
-- getST = do
--   gets (\(x, _, _) -> x)

-- modifySS :: (ScopeStack -> ScopeStack) -> AnalyzerState ()
-- modifySS f = do
--   (varMap, declarationStack, accessStack) <- get
--   put (varMap, f declarationStack, accessStack)

-- getSS :: AnalyzerState ScopeStack
-- getSS = do
--   gets (\(_, y, _) -> y)

-- modifyAS :: (AccessStack -> AccessStack) -> AnalyzerState ()
-- modifyAS f = do
--   (varMap, declarationStack, accessStack) <- get
--   put (varMap, declarationStack, f accessStack)

-- getAS :: AnalyzerState AccessStack
-- getAS = gets (\(_, _, z) -> z)

-- currentScope :: AnalyzerState Scope
-- currentScope = do
--   scopes <- getSS
--   case scopes of
--     [] -> throwError "ERROR"
--     (x:_) -> return x 

-- symbolIsInScope :: String -> AnalyzerState Bool
-- symbolIsInScope ident = elem ident scope <$> currentScope

-- addSymbol :: String -> SymbolInfo -> AnalyzerState ()
-- addSymbol name symbol = do
--   if symbolIsInScope name then 
--     throwError "ERROR"
--   else do
--     modifySS ()
    



-- getName :: Ident-> String
-- getName (Ident name) = name

-- getTypes :: [Argument a] -> [Type a]
-- getTypes arguments = let
--   getType (Argument _ t _) = t
--   in
--     map getType arguments

-- analyzeFunction :: Function a -> AnalyzerState ()
-- analyzeFunction (Function _ returnType ident arguments block) = undefined
  

-- analyzeProgram :: Program a -> AnalyzerState ()
-- analyzeProgram (Program _ []) = return ()
-- analyzeProgram (Program _ functions) = foldM (\_ y -> analyzeFunction y) () functions
