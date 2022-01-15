module MachineCode.Generator.Utilities where

import Control.Monad.State
import Control.Monad.Except
import Errors
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Types
import qualified IntermediateCode.Definitions.Quadruples as Q 
import qualified MachineCode.Generator.Context as C
import Lens.Micro.Platform
import Debug.Trace
import qualified Data.Map as Map
import MachineCode.Types
import Data.Char
import Text.Printf
import Data.List



-- 
-- Global generator
-- 

getNewStringNumber :: C.GlobalGenerator Int
getNewStringNumber = do
  newStringNumber <- view C.stringCounter <$> get 
  modify $ over C.stringCounter (1+)
  return newStringNumber

getStringLabel :: String -> C.GlobalGenerator Label
getStringLabel string = do 
  maybeIndex <- Map.lookup string . view C.strings <$> get
  case maybeIndex of  
    Just stringNumber -> return $ "strings_" ++ show stringNumber
    Nothing -> do
      newStringNumber <- getNewStringNumber
      modify $ over C.strings (Map.insert string newStringNumber)
      return $ "strings_" ++ show newStringNumber
    
emitLine :: String -> C.GlobalGenerator ()
emitLine line = modify $ over C.code (line:)

emitEmptyLine :: C.GlobalGenerator ()
emitEmptyLine = emitLine ""

emitConst :: Label -> String -> [String] -> C.GlobalGenerator ()
emitConst label op arguments = do
  let line = (printf "%-12s" label) ++ op ++ " " ++ (intercalate "," $ arguments)
  emitLine line

emitLabel :: Label -> C.GlobalGenerator ()
emitLabel label = emitLine $ label ++ ":"

emitInstruction :: Show a => String -> [a] -> C.GlobalGenerator ()
emitInstruction mnemonic arguments = do
  let line = printf "%-12s" mnemonic ++ intercalate ", " (map show arguments)
  emitLine $ "\t\t" ++ line

emitGlobal :: Label -> C.GlobalGenerator ()
emitGlobal label = emitInstruction "GLOBAL" [LabeledMemory label]

emitExtern :: Label -> C.GlobalGenerator ()
emitExtern label = emitInstruction "EXTERN" [LabeledMemory label]

emitSection :: Section -> C.GlobalGenerator ()
emitSection section = emitInstruction "SECTION" [section]
-- 
-- Function generator 
-- 
getNewLabelNumber :: C.FunctionGenerator Int
getNewLabelNumber = do
  newLabelNumber <- view C.labelCounter <$> get
  modify $ over C.labelCounter (1+)
  return newLabelNumber

getFunctionEndLabel :: C.FunctionGenerator Label
getFunctionEndLabel = do
  functionIdent <- view C.functionIdent <$> get
  return $ functionIdent ++ "_end"

emitFunctionLabel :: C.FunctionGenerator ()
emitFunctionLabel = do
  functionIdent <- view C.functionIdent <$> get
  lift $ emitLabel functionIdent

emitFunctionEndLabel :: C.FunctionGenerator ()
emitFunctionEndLabel = do
  endLabel <- getFunctionEndLabel
  lift $ emitLabel $ endLabel

getNewLabel :: C.FunctionGenerator Label
getNewLabel = do
  functionIdent <- view C.functionIdent <$> get
  newLabelNumber <- view C.labelCounter <$> get
  modify $ over C.labelCounter (1+)
  return $ functionIdent ++ "_L_" ++ show newLabelNumber

getBlockLabel :: Q.BlockNumber -> C.FunctionGenerator Label
getBlockLabel blockNumber = do
  functionIdent <- view C.functionIdent <$> get
  return $ functionIdent ++ "_" ++ show blockNumber

emitBlockLabel :: Q.BlockNumber -> C.FunctionGenerator ()
emitBlockLabel blockNumber = do
  label <- getBlockLabel blockNumber
  lift $ emitLabel label

getBlock :: Q.BlockNumber -> C.FunctionGenerator Q.Block
getBlock blockNumber = do
  maybeBlock <- Map.lookup blockNumber . view C.blocks <$> get
  case maybeBlock of 
    Nothing -> throwError $ InternalCompilerError "Not able to find block with given block number"
    Just block -> return block
