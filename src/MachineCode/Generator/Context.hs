{-# LANGUAGE TemplateHaskell #-}
module MachineCode.Generator.Context where

import Control.Monad.State
import Errors
import Types
import qualified IntermediateCode.Definitions.Quadruples as Q
import Debug.Trace
import Data.List
import Lens.Micro.Platform
import qualified Data.Map as Map

data FunctionContext 
  = FunctionContext {
    _labelCounter :: Int,
    _functionIdent :: Ident,
    _frameSize :: Int,
    _labels :: Map.Map Q.BlockNumber Label
  }

data GlobalContext 
  = GlobalContext {
    _stringCounter :: Int, 
    _strings :: Map.Map String Index,
    _code :: [String]
  }

$(makeLenses ''GlobalContext)
$(makeLenses ''FunctionContext)

type GlobalGenerator = StateT GlobalContext (Either LatteError)
type FunctionGenerator = StateT FunctionContext GlobalGenerator
-- 
-- Functions
-- 
emptyGlobalContext :: GlobalContext
emptyGlobalContext = GlobalContext 0 Map.empty []

emptyFunctionContext :: Q.FunctionDefinition -> FunctionContext
emptyFunctionContext functionDefinition = let
    fi = view Q.functionIdent functionDefinition
    bs = view Q.blocks functionDefinition
    rc = view Q.registerCounter functionDefinition
  in
    FunctionContext 0 fi rc Map.empty

