{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module IntegrationTests.Generator where

import Test.Tasty ( testGroup, TestTree )
import Data.List ( sort )
import Shelly ( hasExt, shelly, findWhen )
import System.FilePath ( dropExtension, takeBaseName )
import Test.Tasty.HUnit ( testCase, (@?) )

data TestType
  = Good
  | Bad
  deriving (Eq, Show)

type CodeAndOutput = (String, Maybe String)
type IntegrationTest = CodeAndOutput -> Bool
type Directory = (String, TestType)

directories :: [Directory]
directories = [
  ("core", Good),
  ("core_bad", Bad)
  ]

getDirectoryPath :: String -> String
getDirectoryPath = (++) "test/IntegrationTests/"

integrationTestGenerator :: IntegrationTest -> String -> IO TestTree
integrationTestGenerator integrationTest testNameSufix = do
  let testName = "Integration tests - " ++ testNameSufix
  testSuites <- mapM (integrationTestSuiteGenerator integrationTest) directories
  return $ testGroup testName testSuites

integrationTestSuiteGenerator :: IntegrationTest -> Directory -> IO TestTree
integrationTestSuiteGenerator integrationTest (dirname, testType) = do
  files <- shelly $ sort <$> findWhen (pure . hasExt ".lat") (getDirectoryPath dirname)
  testCases <- mapM (integrationTestCaseGenerator integrationTest testType) files
  return $ testGroup dirname testCases

integrationTestCaseGenerator :: IntegrationTest -> TestType -> FilePath  -> IO TestTree
integrationTestCaseGenerator integrationTest Bad filePath = do
  fileContent <- readFile filePath
  let basename = takeBaseName filePath
  let testResult = integrationTest (fileContent, Nothing)
  return $ testCase basename (testResult @? "Integration test failed")
integrationTestCaseGenerator integrationTest Good filePath = do
  fileContent <- readFile filePath
  let basename = takeBaseName filePath
  outputContent <- readFile $ dropExtension filePath ++ ".output"
  let testResult = integrationTest (fileContent, Just outputContent)
  return $ testCase basename (testResult @? "Integration test failed")
