module IntermediateCode.Definitions.AbstractSyntaxTree where

import Types ( Argument, Position, Ident, Type, Value, Operation, CompareOperation )


-- Definitions
type Program = Program' Position
data Program' a = Program a [Function' a]
  deriving (Eq, Ord, Show, Read)

type Function = Function' Position
data Function' a = Function a Type Ident [Argument] (Block' a)
  deriving (Eq, Ord, Show, Read)

type Block = Block' Position
data Block' a = Block a [Statement' a]
  deriving (Eq, Ord, Show, Read)

type Statement = Statement' Position
data Statement' a
    = Empty a
    | InnerBlock a (Block' a)
    | Declaration a Type [Declaration' a]
    | Assigment a Ident (Expression' a)
    | Increment a Ident
    | Decrement a Ident
    | Return a (Expression' a)
    | VoidReturn a
    | If a (Expression' a) (Statement' a)
    | IfElse a (Expression' a) (Statement' a) (Statement' a)
    | While a (Expression' a) (Statement' a)
    | Expression a (Expression' a)
  deriving (Eq, Ord, Show, Read)

type Declaration = Declaration' Position
data Declaration' a = NoInit a Ident | Init a Ident (Expression' a)
  deriving (Eq, Ord, Show, Read)

type Expression = Expression' Position
data Expression' a
    = Variable a Ident
    | Value a Value
    | Application a Ident [Expression' a]
    | Neg a (Expression' a)
    | Not a (Expression' a)
    | Operation a (Expression' a) Operation (Expression' a)
    | Compare a (Expression' a) CompareOperation (Expression' a)
  deriving (Eq, Ord, Show, Read)


-- Instances
instance Functor Program' where
  fmap f (Program a functions) = Program (f a) (map (fmap f) functions)

instance Functor Function' where
  fmap f (Function a _type ident arguments block) = Function (f a) _type ident arguments (fmap f block)

instance Functor Block' where
  fmap f (Block a statements) = Block (f a) (map (fmap f) statements)

instance Functor Statement' where
  fmap f x = case x of
    Empty a -> Empty (f a)
    InnerBlock a block -> InnerBlock (f a) (fmap f block)
    Declaration a _type declarations -> Declaration (f a) _type (map (fmap f) declarations)
    Assigment a ident expr -> Assigment (f a) ident (fmap f expr)
    Increment a ident -> Increment (f a) ident
    Decrement a ident -> Decrement (f a) ident
    Return a expr -> Return (f a) (fmap f expr)
    VoidReturn a -> VoidReturn (f a)
    If a expr statement -> If (f a) (fmap f expr) (fmap f statement)
    IfElse a expr first second -> IfElse (f a) (fmap f expr) (fmap f first) (fmap f second)
    While a expr statement -> While (f a) (fmap f expr) (fmap f statement)
    Expression a expr -> Expression (f a) (fmap f expr)

instance Functor Declaration' where
    fmap f x = case x of
        NoInit a ident -> NoInit (f a) ident
        Init a ident expr -> Init (f a) ident (fmap f expr)

instance Functor Expression' where
    fmap f x = case x of
        Variable a ident -> Variable (f a) ident
        Value a value -> Value (f a) value
        Application a ident exprs -> Application (f a) ident (map (fmap f) exprs)
        Neg a expr -> Neg (f a) (fmap f expr)
        Not a expr -> Not (f a) (fmap f expr)
        Operation a expr1 op expr2 -> Operation (f a) (fmap f expr1) op (fmap f expr2)
        Compare a expr1 op expr2 -> Compare (f a) (fmap f expr1) op (fmap f expr2)
