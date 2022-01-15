module CommandLine where

import Options.Applicative
import qualified Optimizations.Options as O

data Format
  = Assembly
  | DebugQuadruples
  | Quadruples
  deriving (Show)

data CommandLineOptions 
  = CommandLineOptions { 
    inputPath :: FilePath,
    format :: Format,
    optimizerOptions :: O.OptimizerOptions,
    toStdout :: Bool
  }
  deriving (Show)

inputParser :: Parser FilePath
inputParser = let 
    fileInputParser = strOption (long "file" <> metavar "FILENAME" <> help "Input file")
    argumentParser = argument str (metavar "FILE")
  in
    fileInputParser <|> argumentParser

formatParser :: Parser Format
formatParser = let
    assembly = flag' Assembly (long "assembly" <> help "Output assembly code")
    quadruples = flag' Quadruples (long "quadruples" <> help "Output quadruples code")
    debugQuadruples = flag' DebugQuadruples (long "debug-quadruples" <> help "Output quadruples code with debug information")
  in
    assembly <|> quadruples <|> debugQuadruples <|> pure Assembly

optimizerOptionsParser :: Parser O.OptimizerOptions
optimizerOptionsParser = let
    noOptimizations = flag' O.noOptimizations (long "O0")
    basicOptimizations = flag' O.basicOptimizations (long "O1")
    intermediateOptimizations = flag' O.intermediateOptimizations (long "O2")
    allOptimizations = flag' O.allOptimizations (long "O3")
    deadBlocks = switch (long "Odead-blocks") <|> pure False
    mergeBlocks = switch (long "Omerge-blocks") <|> pure False
    optimizePhi = switch (long "Ophi") <|> pure False
    deadCode = switch (long "Odead-code") <|> pure False
    copyPropagations = switch (long "Ocopy-propagations") <|> pure False
    gcse = switch (long "Ogcse") <|> pure False
    optimizerIterations = option auto (long "optimizer-iterations" <> showDefault <> value 100 <> metavar "INT")
    customOptions = O.OptimizerOptions 
                      <$> deadBlocks 
                      <*> mergeBlocks 
                      <*> optimizePhi 
                      <*> copyPropagations
                      <*> deadCode
                      <*> gcse
  in
    (noOptimizations 
    <|> basicOptimizations 
    <|> intermediateOptimizations 
    <|> allOptimizations 
    <|> customOptions) <*> optimizerIterations

commandLineOptionsParser :: Parser CommandLineOptions
commandLineOptionsParser = 
  CommandLineOptions 
  <$> inputParser 
  <*> formatParser
  <*> optimizerOptionsParser
  <*> switch (long "stdout" <> help "Write code to stdout")

parseCommandLineOptions :: IO CommandLineOptions
parseCommandLineOptions = let
    description = fullDesc 
                  <> progDesc "Compile .lat files to assembly code"
                  <> header "Latte compiler"
  in
    execParser $ info (commandLineOptionsParser <**> helper) description