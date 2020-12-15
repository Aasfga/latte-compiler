module Parser.Converter where

import Parser.AbsLatte as Src
import AbstractSyntax.Definitions as Dest

convertProgram :: Src.Program a -> Dest.Program a
convertProgram (Src.Program a functions) = 
  Dest.Program a (map convertFunction functions)

convertFunction :: Src.Function a -> Dest.Function a
convertFunction (Src.Function a _type ident arguments block) = 
  Dest.Function a (convertType _type) (convertIdent ident) (map convertArgument arguments) (convertBlock block)

convertType :: Src.Type a -> Dest.Type
convertType (Src.Int _) = Dest.Int
convertType (Src.Str _) = Dest.String
convertType (Src.Bool _) = Dest.Bool
convertType (Src.Void _) = Dest.Void
convertType (Src.Fun _ retType argTypes) = 
  Dest.Fun (convertType retType) (map convertType argTypes)

convertIdent :: Src.Ident -> String
convertIdent (Ident ident) = ident

convertArgument :: Src.Argument a -> Dest.Argument a
convertArgument (Src.Argument a _type ident) = 
  Dest.Argument a (convertType _type) (convertIdent ident)
  
convertBlock :: Src.Block a -> Dest.Block a
convertBlock (Src.Block a statements) = 
  Dest.Block a (map convertStatement statements)

convertStatement :: Src.Statement a -> Dest.Statement a
convertStatement (Src.Empty a) = 
  Dest.Empty a
convertStatement (Src.InnerBlock a block) = 
  Dest.InnerBlock a (convertBlock block)
convertStatement (Src.Decl a _type declarations) = 
  Dest.Declaration a (convertType _type) (map convertDeclaration declarations)
convertStatement (Src.Ass a ident expr) =
  Dest.Assigment a (convertIdent ident) (convertExpression expr)
convertStatement (Src.Incr a ident) =
  Dest.Increment a (convertIdent ident)
convertStatement (Src.Decr a ident) =
  Dest.Decrement a (convertIdent ident)
convertStatement (Src.Return a expr) = 
  Dest.Return a (convertExpression expr)
convertStatement (Src.VoidReturn a) = 
  Dest.VoidReturn a
convertStatement (Src.If a expr statement) =
  Dest.If a (convertExpression expr) (convertStatement statement)
convertStatement (Src.IfElse a expr first second) =
  Dest.IfElse a (convertExpression expr) (convertStatement first) (convertStatement second)
convertStatement (Src.While a expr statement) =
  Dest.While a (convertExpression expr) (convertStatement statement)
convertStatement (Src.SExp a expr) =
  Dest.Expression a (convertExpression expr)
 
convertExpression :: Src.Expr a -> Dest.Expression a
convertExpression (Src.Var a ident) = 
  Dest.Variable a (convertIdent ident)
convertExpression (Src.LitInt a value) = 
  Dest.IntValue a (fromIntegral value)
convertExpression (Src.LitTrue a) = 
  Dest.BoolValue a True
convertExpression (Src.LitFalse a) = 
  Dest.BoolValue a False
convertExpression (Src.App a ident exprs) =
  Dest.Application a (convertIdent ident) (map convertExpression exprs)
convertExpression (Src.String a value) = 
  Dest.StringValue a value
convertExpression (Src.Neg a expr) = 
  Dest.Neg a (convertExpression expr)
convertExpression (Src.Not a expr) = 
  Dest.Not a (convertExpression expr)
convertExpression (Src.Mul a first op second) = 
  Dest.Operation a (convertExpression first) (convertMulOp op) (convertExpression second)
convertExpression (Src.Add a first op second) = 
  Dest.Operation a (convertExpression first) (convertAddOp op) (convertExpression second)
convertExpression (Src.Rel a first op second) = 
  Dest.Compare a (convertExpression first) (convertRelOp op) (convertExpression second)
convertExpression (Src.And a first second) = 
  Dest.Operation a (convertExpression first) Dest.And (convertExpression second)
convertExpression (Src.Or a first second) = 
  Dest.Operation a (convertExpression first) Dest.Or (convertExpression second)


convertMulOp :: Src.MulOp a -> Dest.Operation
convertMulOp (Src.Times _) = Dest.Times
convertMulOp (Src.Div _) = Dest.Div
convertMulOp (Src.Mod _) = Dest.Mod

convertAddOp :: Src.AddOp a -> Dest.Operation
convertAddOp (Src.Plus _) = Dest.Plus  
convertAddOp (Src.Minus _) = Dest.Minus

convertRelOp :: Src.RelOp a -> Dest.CompareOperation
convertRelOp (Src.LTH _) = Dest.LTH
convertRelOp (Src.LE _) = Dest.LE
convertRelOp (Src.GTH _) = Dest.GTH
convertRelOp (Src.GE _) = Dest.GE
convertRelOp (Src.EQU _) = Dest.EQU
convertRelOp (Src.NE _) = Dest.NE

convertDeclaration :: Src.Declaration a -> Dest.Declaration a
convertDeclaration (Src.NoInit a ident) = 
  Dest.NoInit a (convertIdent ident)
convertDeclaration (Src.Init a ident expr) = 
  Dest.Init a (convertIdent ident) (convertExpression expr)

convert :: Src.Program a -> Dest.Program a
convert = convertProgram