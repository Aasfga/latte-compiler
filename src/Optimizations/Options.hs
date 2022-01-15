{-# LANGUAGE TemplateHaskell #-}
module Optimizations.Options where

import Lens.Micro.Platform

data OptimizerOptions
  = OptimizerOptions {
    _deadBlocks :: Bool,
    _mergeBlocks :: Bool,
    _optimizePhi :: Bool,
    _copyPropagations :: Bool,
    _deadCodeElimination :: Bool,
    _gcse :: Bool,
    _optimizerIterations :: Int
  }
  deriving (Show)


basicOptimizations :: Int -> OptimizerOptions
basicOptimizations = OptimizerOptions True True True True True False

intermediateOptimizations :: Int -> OptimizerOptions
intermediateOptimizations = OptimizerOptions True True True True True True

allOptimizations :: Int -> OptimizerOptions
allOptimizations = OptimizerOptions True True True True True True

noOptimizations :: Int -> OptimizerOptions
noOptimizations = OptimizerOptions False False False False False False

$(makeLenses ''OptimizerOptions)
