
module IntermediateCode.Transformer where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Errors
import Types
import IntermediateCode.Definitions.Quadruples
import IntermediateCode.Definitions.AbstractSyntaxTree

-- State functions
data FunctionContext = FunctionContext

type GlobalTransformer = StateT QuadruplesCode (Either LatteError)
type FunctionTransformer = StateT FunctionContext (Either LatteError)

emptyQuadruplesCode :: QuadruplesCode
emptyQuadruplesCode =
  let 
    functions = Map.empty
  in
    Quadruples functions

-- Transformer
transformProgram :: Program Position -> GlobalTransformer ()
transformProgram = undefined

transformToQuadruples :: Program Position -> Either LatteError QuadruplesCode
transformToQuadruples program = 
  let
    emptyCode = emptyQuadruplesCode
    result = runStateT (transformProgram program) emptyCode
  in case result of
    Left error -> Left error
    Right (_, code) -> Right code

