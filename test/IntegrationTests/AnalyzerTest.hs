module IntegrationTests.AnalyzerTest where 

import Errors ( CompilerError )
import Parser.Parser ( parse )
import Data.Maybe ( isJust, isNothing )
import IntegrationTests.Generator (IntegrationTest)
import IntermediateCode.Transformer
import Control.Monad.Except
import Types

runner :: String -> Either CompilerError ()
runner code = 
  case parse code of
    Left error -> throwError error
    Right ast -> do
      transformToQuadruples ast
      return ()


analyzerIntegrationTest :: IntegrationTest
analyzerIntegrationTest (code, output) = 
  let
    result = runner code
  in case result of
    Left _ -> isNothing output
    Right _ -> isJust output