module Main where

import Parser.Parser ( parse )
import System.IO
import System.Exit
import IntermediateCode.Transformer
import IntermediateCode.Definitions.Quadruples


main :: IO ()
main = do
  content <- getContents
  case parse content of
    Left latteError -> do
      hPutStrLn stderr "ERROR\n"
      print latteError
      exitWith $ ExitFailure 1
    Right abstractSyntax -> do
      case transformToQuadruples abstractSyntax of
        Left latteError -> do
          hPutStrLn stderr "ERROR\n"
          print latteError
          exitWith $ ExitFailure 1
        Right quadruples -> do
          putStrLn $ unlines $ getCode quadruples
          -- hPutStrLn stderr $ show quadruples
          -- hPutStrLn stderr "OK\n"