module Analyzer.Leftovers where

import Types

isStringOperation :: Operation -> Bool
isStringOperation Plus = True
isStringOperation _ = False

isIntOperation :: Operation -> Bool
isIntOperation Plus = True
isIntOperation Minus = True
isIntOperation Times = True
isIntOperation Div = True
isIntOperation Mod = True
isIntOperation _ = False

isBoolOperation :: Operation -> Bool
isBoolOperation And = True
isBoolOperation Or = True
isBoolOperation _ = False

isCorrectCompare :: Type -> Type -> Bool
isCorrectCompare Int Int = True
isCorrectCompare String String = True
isCorrectCompare Bool Bool = True
isCorrectCompare _ _ = False

isInt :: Type -> Bool
isInt Int = True
isInt _ = False

isBool :: Type -> Bool
isBool Bool = True
isBool _ = False

libraryFunctionsOld :: [(String, Type)]
libraryFunctionsOld = [
    ("printInt", Function Void [Int]),
    ("printString", Function Void [String]),
    ("error", Function Void []),
    ("readInt", Function Int []),
    ("readString", Function String [])
  ]
  