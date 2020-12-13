module Parser.Parser where 

import Parser.ParLatte ( pProgram, myLexer )
import Parser.ErrM ( Err(Ok, Bad) )
import Parser.Converter ( convert )
import qualified Parser.AbsLatte as AbsLatte
import AbstractSyntax.Definitions ( Program )
import Parser.PrintLatte ( Print, printTree )

_parse :: String -> Err (AbsLatte.Program ())
_parse string = pProgram (myLexer string)

parse :: String -> Either String (Program ())
parse string = 
  case _parse string of 
    Bad msg -> Left msg
    Ok tree -> Right $ convert tree

showTree :: (Show a, Print a) => a -> IO ()
showTree tree
 = do
      putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrLn $ "\n[Linearized tree]\n\n" ++ printTree tree