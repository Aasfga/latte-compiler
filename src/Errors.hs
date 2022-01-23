{-# LANGUAGE FlexibleInstances #-}
module Errors where
import Data.List
import Types

data LatteError 
  = ParserError String
  | InternalCompilerError String 
  -- Analyzer errors
  | SymbolNotFound Ident
  | NotAnArray Type
  | NotAnObject Type
  | NotAClass Type
  | ClassMemberNotFound Ident Ident
  | SymbolInScope Ident
  | FunctionNotFound Ident
  | TypeMissmatchApplication Ident [Type] [Type] 
  | TypeMissmatchUnaryOperator Type String 
  | TypeMissmatchBinaryOperator Type Type Operation
  | TypeMissmatchCompare Type Type
  | TypeMissmatchAssigment Type Type
  | TypeMissmatch Type Type
  | TypeMissmatchIf Type
  | TypeMissmatchReturn Ident Type Type
  | MissingReturn Ident Type
  | IntegerOutOfBound Integer

data CompilerError 
  = CompilerError LatteError Position
-- 
-- Instances
--
instance Show LatteError where   
  show (ParserError msg) = 
    "Parser error." ++ show msg
  show (SymbolNotFound ident) = 
    "Symbol " ++ ident ++ " not found"
  show (NotAnArray _type) = "Value is not an array. " ++ show _type
  show (NotAnObject _type) = show _type ++ " is not an object"
  show (NotAClass _type) = show _type ++ " is not a class type"
  show (ClassMemberNotFound classIdent memberIdent) = 
    "Member " ++ memberIdent ++ " in class " ++ classIdent ++ " not found"
  show (SymbolInScope ident) = 
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
  show (TypeMissmatch required found) = 
    "Incompatible array types. " ++
    show found ++ " is different than " ++ show required
  show (TypeMissmatchIf _type) = 
    "Not able to determine bool value from value of type " ++ show _type
  show (TypeMissmatchReturn ident required found) = 
    "Wrong return type in function "++ ident ++ ":\n" ++ 
    "Required: " ++ show required ++ "\n" ++ 
    "Found: " ++ show found
  show (MissingReturn ident _type) = 
    "Function " ++ ident ++ " should return value of type " ++ show _type ++ " but returns nothing"
  show (IntegerOutOfBound x) = 
    "Integer " ++ show x ++ " is out of bound"  
  show (InternalCompilerError msg) = 
    "Internal compiler error. " ++ msg

instance Show CompilerError where
  show (CompilerError errorCategory NoPosition) = show errorCategory
  show (CompilerError errorCategory (Position line column)) = show line ++ ":" ++ show column ++ ": " ++ show errorCategory
  
