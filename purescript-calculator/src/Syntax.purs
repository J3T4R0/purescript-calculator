module Syntax where

import Prelude

data Expr =
    Lit Lit
  | Var Name
  | BinOp BinOp Expr Expr
  | UnOp UnOp Expr

type Name = String

data BinOp = Add | Sub | Mul | Div | Less | Eql | Or

data UnOp = Negate | Not

data Lit = Int Int | Bool Boolean

instance showExpr ∷ Show Expr where
  show (Lit l) = show l
  show (Var n) = n
  show (BinOp op e1 e2) = show e1 <> show op <> show e2
  show (UnOp op e2) = show op <> show e2

instance showLit ∷ Show Lit where
  show (Int n) = show n
  show (Bool b) = show b

instance showBinOp ∷ Show BinOp where
  show Add = " + "
  show Sub = " - "
  show Mul = "*"
  show Div = "/"
  show Less = "<"
  show Eql = "="
  show Or = "|"

instance showUnOp ∷ Show UnOp where
  show Negate = "-"
  show Not = "~"

derive instance eqExpr ∷ Eq Expr
derive instance eqBinop :: Eq BinOp
derive instance eqUnop :: Eq UnOp
derive instance eqLit :: Eq Lit

data Cmd = Assign Name Expr | Eval Expr

infix 0 Assign as :=

typeof :: Lit -> String
typeof (Int _) = "int"
typeof (Bool _) = "bool"
