module FunFlow.Ast where

data Op
  = Add | Sub | Mul | Div
  deriving (Eq,Show)

type Pi    = Int -- For numbering lambda's etc. that can then be tracked in the analysis
type Name  = String  -- For identifier names

data Expr
  = Int Int
  | Bool    Bool
  | Pair    Pi   Expr Expr
  | PCase   Expr Name Name Expr
  | Cons    Pi   Expr Expr
  | Nil     Pi
  | LCase   Expr Name Name Expr Expr
  | DType   Pi   [Expr]
  | DTCase  Expr [Name] Expr Expr
  | Var     Name
  | Fun     Pi   Name Name Expr
  | Fn      Pi   Name Expr
  | App     Expr Expr
  | Let     Name Expr Expr
  | ITE     Expr Expr Expr
  | Oper    Op   Expr Expr
  deriving (Eq,Show)

bin :: Name -> Expr -> Expr -> Expr
bin op x y = Oper r x y where
  r = case op of
        "+" -> Add
        "-" -> Sub
        "*" -> Mul
        "/" -> Div
        _   -> error "Undefined binary operator"


