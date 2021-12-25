
module IntermediateCode.Transformer where

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Except
import Errors
import Types
import IntermediateCode.Definitions.Quadruples
import IntermediateCode.Definitions.AbstractSyntaxTree

-- State functions

data OuterContext = OuterContext
data InnerContext = InnerContext

type OuterState = StateT OuterContext (Either LatteError)
type InnerState = StateT InnerContext (Either LatteError)

-- Transformer

transformToQuadruples :: Program Position -> QuadruplesCode
transformToQuadruples = undefined 



