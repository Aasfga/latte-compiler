module Main where

import Parser.Parser ( parse )
import System.IO
import System.Exit
import IntermediateCode.Transformer
import qualified IntermediateCode.Definitions.Quadruples as Q 
import MachineCode.Generator
import Errors
import Types
import CommandLine
import System.FilePath.Posix
import qualified Optimizations.Options as O
import Optimizations.QuadruplesOptimizer


writeOutput :: Bool -> String -> String -> IO ()
writeOutput False filepath code = writeFile filepath code
writeOutput True _ code = putStrLn code

getOutputFilepath :: Format -> String -> String
getOutputFilepath (Assembly) filepath = filepath -<.> ".s"
getOutputFilepath (Quadruples) filepath = filepath -<.> ".q"
getOutputFilepath (DebugQuadruples) filepath = filepath -<.> ".dq"

getQuadruples :: O.OptimizerOptions -> String -> Either CompilerError String
getQuadruples options sourceCode = do
  ast <- parse sourceCode 
  quadruples <- transformToQuadruples ast
  optimizedQuadruples <- optimizeQuadruples options quadruples
  return $ show optimizedQuadruples

getDebugQuadruples :: O.OptimizerOptions -> String -> Either CompilerError String
getDebugQuadruples options sourceCode = do
  ast <- parse sourceCode 
  quadruples <- transformToQuadruples ast
  optimizedQuadruples <- optimizeQuadruples options quadruples
  return $ debugShow optimizedQuadruples
  
getAssembly :: O.OptimizerOptions -> String -> Either CompilerError String
getAssembly options sourceCode = do
  ast <- parse sourceCode 
  quadruples <- transformToQuadruples ast
  optimizedQuadruples <- optimizeQuadruples options quadruples
  generateMachineCode optimizedQuadruples

getCompilerFunction :: Format -> O.OptimizerOptions ->  String -> Either CompilerError String
getCompilerFunction Assembly = getAssembly
getCompilerFunction Quadruples = getQuadruples
getCompilerFunction DebugQuadruples = getDebugQuadruples

main :: IO ()
main = do
  CommandLineOptions inputFilepath format optimizerOptions toStdout <- parseCommandLineOptions
  sourceCode <- readFile inputFilepath
  let outputFilepath = getOutputFilepath format inputFilepath
  let compilerFunction = getCompilerFunction format
  case compilerFunction optimizerOptions sourceCode of 
    Left compilerError -> do
      hPutStrLn stderr "Error"
      hPutStrLn stderr $ show compilerError
      exitWith $ ExitFailure 1
    Right code -> do
      hPutStrLn stderr "OK"
      writeOutput toStdout outputFilepath code