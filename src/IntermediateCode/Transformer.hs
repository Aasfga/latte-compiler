
module IntermediateCode.Transformer where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Errors
import Data.Maybe
import Types
import IntermediateCode.Definitions.AbstractSyntaxTree
import IntermediateCode.Definitions.Quadruples
    ( QuadruplesCode, 
      FunctionCode,
      emptyFunction )
import IntermediateCode.TransformerContext
    ( FunctionTransformer, 
      GlobalTransformer,
      FunctionContext, 
      emptyFunctionContext,
      emptyGlobalContext )
import qualified IntermediateCode.Definitions.AbstractSyntaxTree as AST
import qualified IntermediateCode.Definitions.Quadruples as Q 
import qualified IntermediateCode.TransformerContext as C
import Lens.Micro.Platform


-- Transformer
transformBlock :: Block -> FunctionTransformer ()
transformBlock block = do
  newScope
  addArgumentsToScope
  removeScope

transformFunction :: Function -> FunctionTransformer ()
transformFunction (Function p returnType ident arguments block) = do
  transformBlock block


transformFunctionToQuadruples :: Function -> GlobalTransformer ()
transformFunctionToQuadruples function = do
  result <- runFunctionTransformer transformFunction function
  return ()

defineGlobalSymbols :: Program -> GlobalTransformer ()
defineGlobalSymbols (Program _ functions) = do
  mapM_ defineSymbol libraryFunctions 
  mapM_ defineSymbol functions


transformProgram :: Program -> GlobalTransformer ()
transformProgram program = do
  defineGlobalSymbols program
  assertMainExists


transformToQuadruples :: Program -> Either LatteError QuadruplesCode
transformToQuadruples program = case runGlobalTransformer transformProgram program of
    Left error -> Left error
    Right (_, code) -> Right code


-- Utilities

libraryFunctions :: [Function]
libraryFunctions = let 
    position = NoPosition 
    emptyBlock = Block position []
    createArg = \t -> Argument t "x"
  in [
    (Function position Void "printInt" [createArg Int] emptyBlock),
    (Function position Void "printString" [createArg String] emptyBlock),
    (Function position Void "error" [] emptyBlock), 
    (Function position Int "readInt" [] emptyBlock),
    (Function position String "readString" [] emptyBlock)
  ]

-- Global context functions 

assertMainExists :: GlobalTransformer ()
assertMainExists = do
  maybeFunction <- gets $ Map.lookup "main" . view Q.functions
  unless (isJust maybeFunction) (throwError $ SymbolNotFound "main")
  let function = fromJust maybeFunction
  let retType = view Q.returnType function
  let argTypes = map getArgumentType $ view Q.arguments function
  unless (retType == Int && null argTypes) (throwError $ SymbolNotFound "main")

assertCanDefineSymbol :: Ident -> GlobalTransformer ()
assertCanDefineSymbol ident = do
  functionExists <- gets $ Map.member ident . view Q.functions
  let canDefine = not functionExists
  unless canDefine (throwError $ SymbolInScope ident)

defineSymbol :: Function -> GlobalTransformer ()
defineSymbol (Function _ retType ident args _) = do
  assertCanDefineSymbol ident
  let newFunction = emptyFunction retType args
  modify $ over Q.functions (Map.insert ident newFunction)

-- Function context functions 
assertCanDefineVariable :: Ident -> FunctionTransformer ()
assertCanDefineVariable ident = do
  isInScope <- gets $ (elem ident) . view C.scope
  unless (not isInScope) (throwError $ SymbolInScope ident)

defineVariable :: Type -> Ident -> FunctionTransformer ()
defineVariable _type ident = do
  assertCanDefineVariable ident
  modify $ over C.scope (ident:)
  modify $ over C.variables (Map.insertWith (++) ident [_type])

assertScopeNotEmpty :: FunctionTransformer ()
assertScopeNotEmpty = do
  isEmpty <- gets $ null . view C.scope
  unless (not isEmpty) (throwError $ InternalGeneratorError "Scope list is empty")

addArgumentToScope :: Argument -> FunctionTransformer ()
addArgumentToScope (Argument _type ident) = do
  -- save argument to create variable label
  defineVariable _type ident

addArgumentsToScope :: FunctionTransformer ()
addArgumentsToScope = do
  arguments <- gets $ view C.arguments
  mapM_ addArgumentToScope arguments

newScope :: FunctionTransformer ()
newScope = do
  modify $ over C.scope ([]:)

removeScope :: FunctionTransformer ()
removeScope = do
  assertScopeNotEmpty
  modify $ over C.scope tail

assertReturnTypeIsCorrect :: Type -> FunctionTransformer ()
assertReturnTypeIsCorrect actualType = do
  expectedType <- gets $ view C.returnType
  unless (actualType == expectedType) (throwError $ TypeMissmatchReturn "fun" expectedType actualType)

-- Transformers

runGlobalTransformer :: (Program -> GlobalTransformer ()) -> Program -> Either LatteError ((), QuadruplesCode)
runGlobalTransformer transformer program = let
    initialState = emptyGlobalContext 
  in
    runStateT (transformer program) initialState

runFunctionTransformer :: (Function -> FunctionTransformer ()) -> Function -> GlobalTransformer ((), FunctionContext)
runFunctionTransformer transformer function = let
    (Function _ returnType ident arguments _) = function
    initialState = emptyFunctionContext returnType ident arguments 
  in
    runStateT (transformer function) initialState