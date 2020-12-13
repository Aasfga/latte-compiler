module Parser.SkelLatte where

-- Haskell module generated by the BNF converter

import Parser.AbsLatte
import Parser.ErrM
{-# ANN module "HLint: ignore" #-}
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> Result
transIdent x = case x of
  Ident string -> failure x
transProgram :: Show a => Program a -> Result
transProgram x = case x of
  Program _ functions -> failure x
transFunction :: Show a => Function a -> Result
transFunction x = case x of
  Function _ type_ ident arguments block -> failure x
transArgument :: Show a => Argument a -> Result
transArgument x = case x of
  Argument _ type_ ident -> failure x
transBlock :: Show a => Block a -> Result
transBlock x = case x of
  Block _ statements -> failure x
transStatement :: Show a => Statement a -> Result
transStatement x = case x of
  Empty _ -> failure x
  InnerBlock _ block -> failure x
  Decl _ type_ declarations -> failure x
  Ass _ ident expr -> failure x
  Incr _ ident -> failure x
  Decr _ ident -> failure x
  Return _ expr -> failure x
  VoidReturn _ -> failure x
  If _ expr statement -> failure x
  IfElse _ expr statement1 statement2 -> failure x
  While _ expr statement -> failure x
  SExp _ expr -> failure x
transDeclaration :: Show a => Declaration a -> Result
transDeclaration x = case x of
  NoInit _ ident -> failure x
  Init _ ident expr -> failure x
transType :: Show a => Type a -> Result
transType x = case x of
  Int _ -> failure x
  Str _ -> failure x
  Bool _ -> failure x
  Void _ -> failure x
  Fun _ type_ types -> failure x
transExpr :: Show a => Expr a -> Result
transExpr x = case x of
  Var _ ident -> failure x
  LitInt _ integer -> failure x
  LitTrue _ -> failure x
  LitFalse _ -> failure x
  App _ ident exprs -> failure x
  String _ string -> failure x
  Neg _ expr -> failure x
  Not _ expr -> failure x
  Mul _ expr1 mulop expr2 -> failure x
  Add _ expr1 addop expr2 -> failure x
  Rel _ expr1 relop expr2 -> failure x
  And _ expr1 expr2 -> failure x
  Or _ expr1 expr2 -> failure x
transAddOp :: Show a => AddOp a -> Result
transAddOp x = case x of
  Plus _ -> failure x
  Minus _ -> failure x
transMulOp :: Show a => MulOp a -> Result
transMulOp x = case x of
  Times _ -> failure x
  Div _ -> failure x
  Mod _ -> failure x
transRelOp :: Show a => RelOp a -> Result
transRelOp x = case x of
  LTH _ -> failure x
  LE _ -> failure x
  GTH _ -> failure x
  GE _ -> failure x
  EQU _ -> failure x
  NE _ -> failure x

