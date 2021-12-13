module Main where

import Parser.Parser ( parse )
import Analyzer.Analyzer
import System.IO
import System.Exit


main :: IO ()
main = do
  content <- getContents
  case parse content of
    Left latteError -> do
      hPutStrLn stderr "ERROR\n"
      print latteError
      exitWith $ ExitFailure 1
    Right abstractSyntax ->
      case runAnalyzer abstractSyntax of
        Left latteError -> do
          hPutStrLn stderr "ERROR\n"
          print latteError
          exitWith $ ExitFailure 1
        Right _ -> hPutStrLn stderr "OK\n"