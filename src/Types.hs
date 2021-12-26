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