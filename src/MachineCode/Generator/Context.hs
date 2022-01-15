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

type ValueIndex = Int

data FunctionContext 
  = FunctionContext {
    _blocks :: Map.Map Q.BlockNumber Q.Block,
    _labelCounter :: Int,
    _functionIdent :: Ident,
    _frameSize :: Int,
    _labels :: Map.Map Q.BlockNumber Label
  }

data GlobalContext 
  = GlobalContext {
    _stringCounter :: Int, 
    _strings :: Map.Map String Index,
    _code :: [String],
    _stackCounter :: Int
  }

$(makeLenses ''GlobalContext)
$(makeLenses ''FunctionContext)

type GlobalGenerator = StateT GlobalContext (Either LatteError)
type FunctionGenerator = StateT FunctionContext GlobalGenerator
-- 
-- Functions
-- 
emptyGlobalContext :: GlobalContext
emptyGlobalContext = GlobalContext 0 Map.empty [] 0

emptyFunctionContext :: Q.FunctionDefinition -> FunctionContext
emptyFunctionContext functionDefinition = let
    __functionIdent = view Q.functionIdent functionDefinition
    __blocks = view Q.blocks functionDefinition
    __registerCounter = view Q.registerCounter functionDefinition
  in
    FunctionContext __blocks 0 __functionIdent __registerCounter Map.empty

