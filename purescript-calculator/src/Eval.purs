module Eval where

import Prelude
import Syntax (Expr(..), BinOp(..), UnOp(..), Lit(..), typeof)
import Error (Expect, typeMismatch, unknownValue)

import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap, empty, lookup)

type Env = StrMap Expr

initEnv ∷ Env
initEnv = empty

eval :: Env → Expr → Expect Expr
eval env = case _ of
  n@(Lit (Int _)) → pure n
  b@(Lit (Bool _)) → pure b
  Var name → case lookup name env of
    Just val → pure val
    _ → unknownValue name
  BinOp op e1 e2 → evalBinOp env op e1 e2
  UnOp op e → evalUnOp env op e

raiseInt :: Int → Expect Expr
raiseInt = pure <<< Lit <<< Int

raiseBool :: Boolean → Expect Expr
raiseBool = pure <<< Lit <<< Bool

evalBinOp :: Env → BinOp → Expr → Expr → Expect Expr
evalBinOp env op e1 e2 = case op of
  Add → evalArithBinOp (add) env e1 e2
  Sub → evalArithBinOp (sub) env e1 e2
  Mul → evalArithBinOp (mul) env e1 e2
  Div → evalArithBinOp (div) env e1 e2
  Or → evalOr env e1 e2
  Less → evalLess env e1 e2
  Eql → evalEql env e1 e2

type EvalBinOp = Env → Expr → Expr → Expect Expr

evalArithBinOp ∷ (forall s. Semiring s ⇒ EuclideanRing s ⇒ s → s → s) → EvalBinOp
evalArithBinOp op env e1 e2 = case e1, e2 of
  Lit (Int n), Lit (Int m) → raiseInt $ op n m
  Lit t@(Bool _), _ → typeMismatch t "int"
  _, Lit t@(Bool _) → typeMismatch t "int"
  _, _ → do
    e ← eval env e1
    e' ← eval env e2
    evalArithBinOp op env e e'

evalOr ∷ EvalBinOp
evalOr env e1 e2 = case e1, e2 of
  Lit (Bool p), Lit (Bool q) → raiseBool $ p || q
  Lit t@(Int _), _ → typeMismatch t "bool"
  _, Lit t@(Int _) → typeMismatch t "bool"
  _, _ → do
    e ← eval env e1
    e' ← eval env e2
    evalOr env e e'

evalLess ∷ EvalBinOp
evalLess env e1 e2 = case e1, e2 of
  Lit (Int n), Lit (Int m) → raiseBool $ n < m
  Lit (Bool p), Lit (Bool q) → raiseBool $ p < q
  Lit t1, Lit t2 → typeMismatch t2 $ typeof t1
  _, _ → do
    e ← eval env e1
    e' ← eval env e2
    evalLess env e e'

evalEql ∷ EvalBinOp
evalEql env e1 e2 = case e1, e2 of
  Lit x, Lit y → raiseBool $ x == y
  _, _ → do
    e ← eval env e1
    e' ← eval env e2
    evalEql env e e'

type EvalUnOp = Env → Expr → Expect Expr

evalUnOp ∷ Env → UnOp → Expr → Expect Expr
evalUnOp env op e = case op of
  Not → evalNot env e
  Negate → evalNegate env e

evalNot ∷ EvalUnOp
evalNot env = case _ of
  Lit (Bool b) → raiseBool $ not b
  Lit l → typeMismatch l "bool"
  e → eval env e >>= evalNot env

evalNegate ∷ EvalUnOp
evalNegate env = case _ of
  Lit (Int n) → raiseInt (-n)
  Lit l → typeMismatch l "bool"
  e → eval env e >>= evalNegate env
