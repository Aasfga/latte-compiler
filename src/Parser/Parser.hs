module Parser.Parser where

import Parser.BnfcParser.ParLatte ( pProgram, myLexer )
import Parser.BnfcParser.ErrM
import Parser.Converter ( convert )
import qualified Parser.BnfcParser.AbsLatte as AbsLatte
import IntermediateCode.Definitions.AbstractSyntaxTree
import Parser.BnfcParser.PrintLatte ( Print, printTree )
import Types
import Errors

_parse :: String -> Err AbsLatte.Program
_parse string = pProgram (myLexer string)

parse :: String -> Either CompilerError Program
parse string =
  case _parse string of
    Bad msg -> Left $ CompilerError (ParserError msg) NoPosition
    Ok tree -> Right $ convert tree