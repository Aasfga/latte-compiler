module Errors where
import Data.List
import AbstractSyntax.Definitions

data LatteError 
  = AnalyzerError AnalyzerError
  | Xasdafadfsaaasdf Int

data AnalyzerError 
  = SymbolNotFound Ident
  | SymbolInScope Ident Type
  | FunctionNotFound Ident
  | TypeMissmatchApplication Ident [Type] [Type] 
  | TypeMissmatchUnaryOperator Type String 
  | TypeMissmatchBinaryOperator Type Type Operation
  | TypeMissmatchCompare Type Type
  | TypeMissmatchAssigment Type Type
  | TypeMissmatchIf Type
  | TypeMissmatchReturn Ident Type Type
  | MissingReturn Ident Type
  | InternalError String
  | IntegerOutOfBound Int


instance Show AnalyzerError where   
  show (SymbolNotFound ident) = 
    "Symbol " ++ ident ++ " not found"
  show (SymbolInScope ident _type) = 
    "Symbol '" ++ ident ++ "' is already defined"
  show (FunctionNotFound ident) = 
    "Function " ++ ident ++ " not found"
  show (TypeMissmatchApplication ident required found) = let
      parsedRequired = intercalate ", " $ map show required
      parsedFound = intercalate ", " $ map show found
    in
      "Method '" ++ ident ++ "' cannot be applied to given arguments.\n" ++
      "Required: (" ++ parsedRequired ++ ")\n" ++ 
      "Found: (" ++ parsedFound ++ ")"
  show (TypeMissmatchUnaryOperator _type op) =
    "Bad operand type " ++ show _type ++ " for unary operator " ++ op
  show (TypeMissmatchBinaryOperator first second op) = 
    "Bad operand types for binary operator '" ++ show op ++ "'\n" ++
    "First type: " ++ show first ++ "\n" ++ 
    "Second type: " ++ show second
  show (TypeMissmatchCompare first second) = 
    "Not able to compare " ++ show first ++ " with " ++ show second 
  show (TypeMissmatchAssigment required found) = 
    "Incompatible types: " ++ 
    show found ++ " value cannot be assigned to " ++ show required ++ " variable"
  show (TypeMissmatchIf _type) = 
    "Not able to determine bool value from value of type " ++ show _type
  show (TypeMissmatchReturn ident required found) = 
    "Wrong return type in function "++ ident ++ ":\n" ++ 
    "Required: " ++ show required ++ "\n" ++ 
    "Found: " ++ show found
  show (MissingReturn ident _type) = 
    "Function " ++ ident ++ " should return value of type " ++ show _type ++ " but returns nothing"
  show (InternalError msg) = 
    "Internal error. " ++ msg
  show (IntegerOutOfBound x) = 
    "Integer " ++ show x ++ " is out of bound"  