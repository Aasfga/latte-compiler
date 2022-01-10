module Main where

import Parser.Parser ( parse )
import System.IO
import System.Exit
import IntermediateCode.Transformer
import IntermediateCode.Definitions.Quadruples ( getCode )
import MachineCode.Generator
import Errors
import Types

printError :: (LatteError, Position) -> IO ()
printError (latteError, NoPosition) = do
  hPutStrLn stderr "Error"
  hPutStrLn stderr $ show latteError
printError (latteError, Position line column) = do
  hPutStrLn stderr $ "Error in line " ++ show line ++ ", column " ++ show column
  hPutStrLn stderr $ show latteError

printLatteError :: LatteError -> IO ()
printLatteError latteError = do
  hPutStrLn stderr "Error"
  hPutStrLn stderr $ show latteError


main :: IO ()
main = do
  content <- getContents
  case parse content of
    Left latteError -> do
      printLatteError latteError
      exitWith $ ExitFailure 1
    Right abstractSyntax -> do
      case transformToQuadruples abstractSyntax of
        Left latteError -> do
          printError latteError
          exitWith $ ExitFailure 1
        Right quadruples -> do
          -- hPutStrLn stderr $ unlines $ getCode quadruples
          case generateMachineCode quadruples of
            Left latteError -> do
              printLatteError latteError
              exitWith $ ExitFailure 1
            Right c -> do
              putStrLn $ unlines c
              hPutStrLn stderr "OK"