import Test.Tasty
import Test.Tasty.HUnit
import IntegrationTests.Generator
import IntegrationTests.AnalyzerTest
import Data.List
import Data.Ord

main :: IO ()
main = do
    analyzerTests <- integrationTestGenerator analyzerIntegrationTest "Analyzer"
    defaultMain analyzerTests


unitTests = testGroup "Unit tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1,2] @?= GT
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1,2,2] @?= GT
  ]