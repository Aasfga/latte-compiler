{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
module IntermediateCode.Definitions.AbstractSyntaxTree where

import Types ( Argument, Position (..), Ident, Type, Operation, CompareOperation, HasPosition (getPosition), HasIdent (getIdent))


-- Definitions
type Program = Program' Position
data Program' a = Program a [GlobalSymbol' a]
  deriving (Eq, Ord, Show, Read)

type GlobalSymbol = GlobalSymbol' Position
data GlobalSymbol' a 
  = Function a Type Ident [Argument] (Block' a)
  | Class a Ident [ClassMember' a]
  deriving (Eq, Ord, Show, Read)

type ClassMember = ClassMember' Position
data ClassMember' a
  = AttributeDefinition a Type Ident
  deriving (Eq, Ord, Show, Read)

type Block = Block' Position
data Block' a = Block a [Statement' a]
  deriving (Eq, Ord, Show, Read)

type Statement = Statement' Position
data Statement' a
    = Empty a
    | InnerBlock a (Block' a)
    | Declaration a Type [Declaration' a]
    | Assigment a (LValue' a) (Expression' a)
    | Increment a (LValue' a)
    | Decrement a (LValue' a)
    | Return a (Expression' a)
    | VoidReturn a
    | If a (Expression' a) (Statement' a)
    | IfElse a (Expression' a) (Statement' a) (Statement' a)
    | While a (Expression' a) (Statement' a)
    | ForEach a Type Ident (Expression' a) (Statement' a)
    | Expression a (Expression' a)
  deriving (Eq, Ord, Show, Read)

type Declaration = Declaration' Position
data Declaration' a = NoInit a Ident | Init a Ident (Expression' a)
  deriving (Eq, Ord, Show, Read)

type LValue = LValue' Position
data LValue' a
  = Variable a Ident
  | ArrayAccess a (Expression' a) (Expression' a)
  | Attribute a (Expression' a) Ident
  deriving (Eq, Ord, Show, Read)

type Expression = Expression' Position
data Expression' a
    = LValue a (LValue' a)
    | NewObject a Type
    | NewArray a Type (Expression' a)
    | Cast a Type (Expression' a)
    | Value a Value
    | Application a Ident [Expression' a]
    | Neg a (Expression' a)
    | Not a (Expression' a)
    | Operation a (Expression' a) Operation (Expression' a)
    | Compare a (Expression' a) CompareOperation (Expression' a)
  deriving (Eq, Ord, Show, Read)

data Value 
  = IntegerValue Integer
  | BoolValue Bool
  | StringValue String
  | Null
  deriving (Eq, Ord, Show, Read)
-- 
-- Instances
-- 
instance Functor Program' where
  fmap f (Program a functions) = Program (f a) (map (fmap f) functions)

instance Functor GlobalSymbol' where
  fmap f (Function a _type ident arguments block) = Function (f a) _type ident arguments (fmap f block)
  fmap f (Class a ident members) = Class (f a) ident $ map (fmap f) members

instance Functor ClassMember' where
  fmap f (AttributeDefinition a _type ident) = AttributeDefinition (f a) _type ident

instance Functor Block' where
  fmap f (Block a statements) = Block (f a) (map (fmap f) statements)

instance Functor Statement' where
  fmap f x = case x of
    Empty a -> Empty (f a)
    InnerBlock a block -> InnerBlock (f a) (fmap f block)
    Declaration a _type declarations -> Declaration (f a) _type (map (fmap f) declarations)
    Assigment a lvalue expr -> Assigment (f a) (fmap f lvalue) (fmap f expr)
    Increment a lvalue -> Increment (f a) (fmap f lvalue)
    Decrement a lvalue -> Decrement (f a) (fmap f lvalue)
    Return a expr -> Return (f a) (fmap f expr)
    VoidReturn a -> VoidReturn (f a)
    If a expr statement -> If (f a) (fmap f expr) (fmap f statement)
    IfElse a expr first second -> IfElse (f a) (fmap f expr) (fmap f first) (fmap f second)
    While a expr statement -> While (f a) (fmap f expr) (fmap f statement)
    ForEach a _type ident expr statement -> ForEach (f a) _type ident (fmap f expr) (fmap f statement)
    Expression a expr -> Expression (f a) (fmap f expr)

instance Functor Declaration' where
    fmap f x = case x of
        NoInit a ident -> NoInit (f a) ident
        Init a ident expr -> Init (f a) ident (fmap f expr)

instance Functor LValue' where
  fmap f (Variable a ident) = Variable (f a) ident
  fmap f (ArrayAccess a array index) = ArrayAccess (f a) (fmap f array) (fmap f index)
  fmap f (Attribute a object ident) = Attribute (f a) (fmap f object) ident

instance Functor Expression' where
    fmap f x = case x of
        LValue a lvalue -> LValue (f a) (fmap f lvalue)
        NewObject a _type -> NewObject (f a) _type
        NewArray a _type index -> NewArray (f a) _type (fmap f index)
        Cast a _type expression -> Cast (f a) _type (fmap f expression)
        Value a value -> Value (f a) value
        Application a ident exprs -> Application (f a) ident (map (fmap f) exprs)
        Neg a expr -> Neg (f a) (fmap f expr)
        Not a expr -> Not (f a) (fmap f expr)
        Operation a expr1 op expr2 -> Operation (f a) (fmap f expr1) op (fmap f expr2)
        Compare a expr1 op expr2 -> Compare (f a) (fmap f expr1) op (fmap f expr2)

instance HasPosition Program where
  getPosition (Program p _) = p

instance HasPosition GlobalSymbol where
  getPosition (Function p _ _ _ _) = p
  getPosition (Class p _ _) = p

instance HasPosition Block where
  getPosition (Block p _) = p

instance HasPosition Statement where
  getPosition statement = case statement of
    Empty p -> p
    InnerBlock p _ -> p
    Declaration p _ _ -> p
    Assigment p _ _ -> p
    Increment p _ -> p
    Decrement p _ -> p
    Return p _ -> p
    VoidReturn p -> p
    If p _ _ -> p
    IfElse p _ _ _ -> p
    While p _ _ -> p
    ForEach p _ _ _ _ -> p
    Expression p _ -> p

instance HasPosition Declaration where
    getPosition declaration = case declaration of
        NoInit p _ -> p
        Init p _ _ -> p

instance HasPosition LValue where
  getPosition (Variable p _) = p
  getPosition (ArrayAccess p _ _) = p
  getPosition (Attribute p _ _) = p

instance HasPosition Expression where
    getPosition expression = case expression of
        LValue p _ -> p
        NewObject p _ -> p
        NewArray p _ _ -> p
        Cast p _ _ -> p
        Value p _ -> p
        Application p _ _ -> p
        Neg p _ -> p
        Not p _ -> p
        Operation p _ _ _ -> p
        Compare p _ _ _ -> p

instance HasIdent GlobalSymbol where 
  getIdent (Function _ _ ident _ _) = ident
  getIdent (Class _ ident _) = ident
-- 
-- Patterns
-- 
pattern DummyBlock :: Statement -> Block
pattern DummyBlock statement = Block NoPosition [statement]

pattern DummyAssigment :: Ident -> Expression -> Statement
pattern DummyAssigment ident expression = Assigment NoPosition (Variable NoPosition ident) expression

pattern DummyBool :: Bool -> Expression
pattern DummyBool x = Value NoPosition (BoolValue x)

pattern DummyInt :: Integer -> Expression
pattern DummyInt x = Value NoPosition (IntegerValue x)

pattern DummyIfElse :: Expression -> Statement -> Statement -> Statement
pattern DummyIfElse expression first second = IfElse NoPosition expression first second

pattern DummyEmpty :: Statement
pattern DummyEmpty = Empty NoPosition

pattern DummyNot :: Expression -> Expression
pattern DummyNot expression = Not NoPosition expression
