-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

module Parser.AbsLatte where
{-# ANN module "HLint: ignore" #-}

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Read)

data Program a = Program a [Function a]
  deriving (Eq, Ord, Show, Read)

instance Functor Program where
    fmap f x = case x of
        Program a functions -> Program (f a) (map (fmap f) functions)

data Function a = Function a (Type a) Ident [Argument a] (Block a)
  deriving (Eq, Ord, Show, Read)

instance Functor Function where
    fmap f x = case x of
        Function a type_ ident arguments block -> Function (f a) (fmap f type_) ident (map (fmap f) arguments) (fmap f block)

data Argument a = Argument a (Type a) Ident
  deriving (Eq, Ord, Show, Read)

instance Functor Argument where
    fmap f x = case x of
        Argument a type_ ident -> Argument (f a) (fmap f type_) ident

data Block a = Block a [Statement a]
  deriving (Eq, Ord, Show, Read)

instance Functor Block where
    fmap f x = case x of
        Block a statements -> Block (f a) (map (fmap f) statements)

data Statement a
    = Empty a
    | InnerBlock a (Block a)
    | Decl a (Type a) [Declaration a]
    | Ass a Ident (Expr a)
    | Incr a Ident
    | Decr a Ident
    | Return a (Expr a)
    | VoidReturn a
    | If a (Expr a) (Statement a)
    | IfElse a (Expr a) (Statement a) (Statement a)
    | While a (Expr a) (Statement a)
    | SExp a (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Statement where
    fmap f x = case x of
        Empty a -> Empty (f a)
        InnerBlock a block -> InnerBlock (f a) (fmap f block)
        Decl a type_ declarations -> Decl (f a) (fmap f type_) (map (fmap f) declarations)
        Ass a ident expr -> Ass (f a) ident (fmap f expr)
        Incr a ident -> Incr (f a) ident
        Decr a ident -> Decr (f a) ident
        Return a expr -> Return (f a) (fmap f expr)
        VoidReturn a -> VoidReturn (f a)
        If a expr statement -> If (f a) (fmap f expr) (fmap f statement)
        IfElse a expr statement1 statement2 -> IfElse (f a) (fmap f expr) (fmap f statement1) (fmap f statement2)
        While a expr statement -> While (f a) (fmap f expr) (fmap f statement)
        SExp a expr -> SExp (f a) (fmap f expr)

data Declaration a = NoInit a Ident | Init a Ident (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Declaration where
    fmap f x = case x of
        NoInit a ident -> NoInit (f a) ident
        Init a ident expr -> Init (f a) ident (fmap f expr)

data Type a
    = Int a | Str a | Bool a | Void a | Fun a (Type a) [Type a]
  deriving (Eq, Ord, Show, Read)

instance Functor Type where
    fmap f x = case x of
        Int a -> Int (f a)
        Str a -> Str (f a)
        Bool a -> Bool (f a)
        Void a -> Void (f a)
        Fun a type_ types -> Fun (f a) (fmap f type_) (map (fmap f) types)

data Expr a
    = Var a Ident
    | LitInt a Integer
    | LitTrue a
    | LitFalse a
    | App a Ident [Expr a]
    | String a String
    | Neg a (Expr a)
    | Not a (Expr a)
    | Mul a (Expr a) (MulOp a) (Expr a)
    | Add a (Expr a) (AddOp a) (Expr a)
    | Rel a (Expr a) (RelOp a) (Expr a)
    | And a (Expr a) (Expr a)
    | Or a (Expr a) (Expr a)
  deriving (Eq, Ord, Show, Read)

instance Functor Expr where
    fmap f x = case x of
        Var a ident -> Var (f a) ident
        LitInt a integer -> LitInt (f a) integer
        LitTrue a -> LitTrue (f a)
        LitFalse a -> LitFalse (f a)
        App a ident exprs -> App (f a) ident (map (fmap f) exprs)
        String a string -> String (f a) string
        Neg a expr -> Neg (f a) (fmap f expr)
        Not a expr -> Not (f a) (fmap f expr)
        Mul a expr1 mulop expr2 -> Mul (f a) (fmap f expr1) (fmap f mulop) (fmap f expr2)
        Add a expr1 addop expr2 -> Add (f a) (fmap f expr1) (fmap f addop) (fmap f expr2)
        Rel a expr1 relop expr2 -> Rel (f a) (fmap f expr1) (fmap f relop) (fmap f expr2)
        And a expr1 expr2 -> And (f a) (fmap f expr1) (fmap f expr2)
        Or a expr1 expr2 -> Or (f a) (fmap f expr1) (fmap f expr2)

data AddOp a = Plus a | Minus a
  deriving (Eq, Ord, Show, Read)

instance Functor AddOp where
    fmap f x = case x of
        Plus a -> Plus (f a)
        Minus a -> Minus (f a)

data MulOp a = Times a | Div a | Mod a
  deriving (Eq, Ord, Show, Read)

instance Functor MulOp where
    fmap f x = case x of
        Times a -> Times (f a)
        Div a -> Div (f a)
        Mod a -> Mod (f a)

data RelOp a = LTH a | LE a | GTH a | GE a | EQU a | NE a
  deriving (Eq, Ord, Show, Read)

instance Functor RelOp where
    fmap f x = case x of
        LTH a -> LTH (f a)
        LE a -> LE (f a)
        GTH a -> GTH (f a)
        GE a -> GE (f a)
        EQU a -> EQU (f a)
        NE a -> NE (f a)

