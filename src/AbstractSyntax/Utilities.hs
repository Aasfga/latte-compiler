module AbstractSyntax.Utilities where

import AbstractSyntax.Definitions


getArgumentType :: Argument a -> Type
getArgumentType (Argument _ _type _) = _type

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

getCompareFunction :: Ord a => CompareOperation -> a -> a -> Bool
getCompareFunction LTH = (<)
getCompareFunction LE = (<=)
getCompareFunction GTH  = (>)
getCompareFunction GE = (>=)
getCompareFunction EQU = (==)
getCompareFunction NE = (/=)

minInt :: Int
minInt = -2147483648

maxInt :: Int
maxInt = 2147483647

libraryFunctions :: [(String, Type)]
libraryFunctions = [
    ("printInt", Fun Void [Int]),
    ("printString", Fun Void [String]),
    ("error", Fun Void []),
    ("readInt", Fun Int []),
    ("readString", Fun String [])
  ]