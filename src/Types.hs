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
  | Function Type [Type]
  deriving (Eq, Ord, Read)

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
-- 
-- Clasess
-- 
class HasType a where
  getType :: a -> Type

class HasIdent a where 
  getIdent :: a -> Ident

class HasPosition a where
  getPosition :: a -> Position
-- 
-- Instances
-- 
instance Show Type where
  show Int = "int"
  show String = "string"
  show Bool = "bool"
  show Void = "void"
  show (Function _type argTypes) = let
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

instance HasType Argument where 
  getType (Argument _type _) = _type

instance HasIdent Argument where
  getIdent (Argument _ ident) = ident
-- 
-- Functions
-- 
getCompareFunction :: Ord a => CompareOperation -> a -> a -> Bool
getCompareFunction LTH = (<)
getCompareFunction LE = (<=)
getCompareFunction GTH  = (>)
getCompareFunction GE = (>=)
getCompareFunction EQU = (==)
getCompareFunction NE = (/=)

negateCompareOperation :: CompareOperation -> CompareOperation
negateCompareOperation LTH = GE
negateCompareOperation LE = GTH
negateCompareOperation GTH = LE
negateCompareOperation GE = LTH
negateCompareOperation EQU = NE
negateCompareOperation NE = EQU