module IntegrationTests.AnalyzerTest where 

import Errors ( LatteError )
import Parser.Parser ( parse )
import Analyzer.Analyzer ( runAnalyzer )
import Data.Maybe ( isJust, isNothing )
import IntegrationTests.Generator (IntegrationTest)

runner :: String -> Either LatteError ()
runner code = do
  ast <- parse code
  runAnalyzer ast

analyzerIntegrationTest :: IntegrationTest
analyzerIntegrationTest (code, output) = 
  let
    result = runner code
  in case result of
    Left _ -> isNothing output
    Right _ -> isJust output