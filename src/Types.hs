module Types where

import Data.List ( intercalate )

-- Definitions
data Position 
  = NoPosition
  | Position { lineNumber :: Int, columnNumber :: Int }
  deriving (Eq, Ord, Show, Read)

type Index = Int

type Ident = String

type Label = String

data Type 
  = Int 
  | String 
  | Bool 
  | Void 
  | Fun Type [Type]
  deriving (Eq, Ord, Read)

data Value 
  = IntValue Int
  | BoolValue Bool
  | StringValue String
  deriving (Eq, Ord, Show, Read)

data Argument = Argument Type Ident
  deriving (Eq, Ord, Show, Read)

data CompareOperation 
  = LTH 
  | LE 
  | GTH 
  | GE 
  | EQU 
  | NE
  deriving (Eq, Ord, Read)

data Operation 
  = Plus 
  | Minus 
  | Times 
  | Div 
  | Mod 
  | Or
  | And
  deriving (Eq, Ord, Read)

-- Instances
instance Show Type where
  show Int = "int"
  show String = "string"
  show Bool = "bool"
  show Void = "void"
  show (Fun _type argTypes) = let
      parsedArgTypes = intercalate " -> " $ map show argTypes
      parsedType = show _type
    in
      "(" ++ parsedArgTypes ++ " -> " ++ parsedType ++ ")" 

instance Show Operation where 
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Div = "/"
  show Mod = "%"
  show Or = "||"
  show And = "&&"

instance Show CompareOperation where
  show LTH = "<="
  show LE = "<"
  show GTH = ">"
  show GE = ">="
  show EQU = "=="
  show NE = "!="


-- Functions
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

libraryFunctionsOld :: [(String, Type)]
libraryFunctionsOld = [
    ("printInt", Fun Void [Int]),
    ("printString", Fun Void [String]),
    ("error", Fun Void []),
    ("readInt", Fun Int []),
    ("readString", Fun String [])
  ]
  
-- Functions
getArgumentType :: Argument -> Type
getArgumentType (Argument _type _) = _type