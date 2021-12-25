module Parser.Parser where

import Parser.BnfcParser.ParLatte ( pProgram, myLexer )
import Parser.BnfcParser.ErrM
import Parser.Converter ( convert )
import qualified Parser.BnfcParser.AbsLatte as AbsLatte
import AbstractSyntax.Definitions ( Program, Position )
import Parser.BnfcParser.PrintLatte ( Print, printTree )
import Errors

_parse :: String -> Err AbsLatte.Program
_parse string = pProgram (myLexer string)

parse :: String -> Either LatteError (Program Position)
parse string =
  case _parse string of
    Bad msg -> Left $ ParserError msg
    Ok tree -> Right $ convert tree

showTree :: (Show a, Print a) => a -> IO ()
showTree tree
 = do
      putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree