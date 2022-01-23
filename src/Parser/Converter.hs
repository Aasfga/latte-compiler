module Parser.Converter where

import qualified Parser.BnfcParser.AbsLatte as Src
import qualified IntermediateCode.Definitions.AbstractSyntaxTree as Dest
import qualified Types as Types

convertPosition :: Src.BNFC'Position -> Types.Position
convertPosition (Just (line, column)) = Types.Position line column
convertPosition Nothing = Types.NoPosition

convertProgram :: Src.Program' a -> Dest.Program' a
convertProgram (Src.Program a functions) = 
  Dest.Program a (map convertGlobalSymbol functions)

convertGlobalSymbol :: Src.GlobalSymbol' a -> Dest.GlobalSymbol' a
convertGlobalSymbol (Src.Function a _type ident arguments block) 
  = Dest.Function a (convertType _type) (convertIdent ident) (map convertArgument arguments) (convertBlock block)
convertGlobalSymbol (Src.Class a ident members)
  = Dest.Class a (convertIdent ident) $ map convertClassMember members

convertClassMember :: Src.ClassMember' a -> Dest.ClassMember' a
convertClassMember (Src.AttributeDefinition a _type ident) 
  = Dest.AttributeDefinition a (convertType _type) (convertIdent ident)

convertType :: Src.Type' a -> Types.Type
convertType (Src.Int _) = Types.Int
convertType (Src.Str _) = Types.String
convertType (Src.Bool _) = Types.Bool
convertType (Src.Void _) = Types.Void
convertType (Src.Object _ ident) = Types.Object (convertIdent ident)
convertType (Src.Array _ _type) = Types.Array (convertType _type)
convertType (Src.Fun _ retType argTypes) = 
  Types.Function (convertType retType) (map convertType argTypes)

convertIdent :: Src.Ident -> String
convertIdent (Src.Ident ident) = ident

convertArgument :: Src.Argument' a -> Types.Argument
convertArgument (Src.Argument _ _type ident) = 
  Types.Argument (convertType _type) (convertIdent ident)
  
convertBlock :: Src.Block' a -> Dest.Block' a
convertBlock (Src.Block a statements) = 
  Dest.Block a (map convertStatement statements)

convertStatement :: Src.Statement' a -> Dest.Statement' a
convertStatement (Src.Empty a) = 
  Dest.Empty a
convertStatement (Src.InnerBlock a block) = 
  Dest.InnerBlock a (convertBlock block)
convertStatement (Src.Decl a _type declarations) = 
  Dest.Declaration a (convertType _type) (map convertDeclaration declarations)
convertStatement (Src.Ass a lvalue expr) =
  Dest.Assigment a (convertLValue lvalue) (convertExpression expr)
convertStatement (Src.Incr a lvalue) =
  Dest.Increment a (convertLValue lvalue)
convertStatement (Src.Decr a lvalue) =
  Dest.Decrement a (convertLValue lvalue)
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
convertStatement (Src.ForEach a _type ident expression statement) = 
  Dest.ForEach a (convertType _type) (convertIdent ident) (convertExpression expression) (convertStatement statement)
convertStatement (Src.SExp a expr) =
  Dest.Expression a (convertExpression expr)

convertLValue :: Src.LValue' a -> Dest.LValue' a
convertLValue (Src.Variable a ident) 
  = Dest.Variable a (convertIdent ident) 
convertLValue (Src.ArrayAccess a array index) 
  = Dest.ArrayAccess a (convertExpression array) (convertExpression index)
convertLValue (Src.Attribute a object ident)
  = Dest.Attribute a (convertExpression object) (convertIdent ident)
 
convertExpression :: Src.Expr' a -> Dest.Expression' a
convertExpression (Src.Cast a _type expression) = 
  Dest.Cast a (convertType _type) (convertExpression expression)
convertExpression (Src.LValue a lvalue) =
  Dest.LValue a (convertLValue lvalue)
convertExpression (Src.LitInt a value) = 
  Dest.Value a (Dest.IntegerValue value)
convertExpression (Src.LitTrue a) = 
  Dest.Value a (Dest.BoolValue True)
convertExpression (Src.LitFalse a) = 
  Dest.Value a (Dest.BoolValue False)
convertExpression (Src.LitNull a) =
  Dest.Value a (Dest.Null)
convertExpression (Src.App a ident exprs) =
  Dest.Application a (convertIdent ident) (map convertExpression exprs)
convertExpression (Src.String a value) = 
  Dest.Value a (Dest.StringValue value)
convertExpression (Src.NewObject a _type) =
  Dest.NewObject a (convertType _type)
convertExpression (Src.NewArray a _type expression) =
  Dest.NewArray a (convertType _type) (convertExpression expression)
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
  Dest.Operation a (convertExpression first) Types.And (convertExpression second)
convertExpression (Src.Or a first second) = 
  Dest.Operation a (convertExpression first) Types.Or (convertExpression second)


convertMulOp :: Src.MulOp' a -> Types.Operation
convertMulOp (Src.Times _) = Types.Times
convertMulOp (Src.Div _) = Types.Div
convertMulOp (Src.Mod _) = Types.Mod

convertAddOp :: Src.AddOp' a -> Types.Operation
convertAddOp (Src.Plus _) = Types.Plus  
convertAddOp (Src.Minus _) = Types.Minus

convertRelOp :: Src.RelOp' a -> Types.CompareOperation
convertRelOp (Src.LTH _) = Types.LTH
convertRelOp (Src.LE _) = Types.LE
convertRelOp (Src.GTH _) = Types.GTH
convertRelOp (Src.GE _) = Types.GE
convertRelOp (Src.EQU _) = Types.EQU
convertRelOp (Src.NE _) = Types.NE

convertDeclaration :: Src.Declaration' a -> Dest.Declaration' a
convertDeclaration (Src.NoInit a ident) = 
  Dest.NoInit a (convertIdent ident)
convertDeclaration (Src.Init a ident expr) = 
  Dest.Init a (convertIdent ident) (convertExpression expr)

convert :: Src.Program -> Dest.Program
convert = convertProgram . fmap convertPosition