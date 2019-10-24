module Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Error (Expect, parseError)
import Syntax (Lit(..), Expr(..), BinOp(..), UnOp(..), Cmd(..), (:=))
import Text.Parsing.Parser as TP
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Expr (OperatorTable, Assoc(..), Operator(..), buildExprParser)
import Token (token)

type P a = TP.Parser String a

parens :: forall a. P a → P a
parens = token.parens

reservedOp ∷ String → P Unit
reservedOp = token.reservedOp

reserved ∷ String → P Unit
reserved = token.reserved

identifier :: P String
identifier = token.identifier

int ∷ P Lit
int = Int <$> token.integer

bool ∷ P Lit
bool = reserved "true" $> Bool true <|> reserved "false" $> Bool false

lit ∷ P Expr
lit = Lit <$> int <|> Lit <$> bool

var ∷ P Expr
var = Var <$> identifier

term ∷ P Expr → P Expr
term p = parens p <|> lit <|> var

table ∷ OperatorTable Identity String Expr
table =
  [ [ Prefix (reservedOp "~" $> UnOp Not)
    , Prefix (reservedOp "-" $> UnOp Negate) ]
  , [ Infix (reservedOp "*" $> BinOp Mul) AssocLeft
    , Infix (reservedOp "/" $> BinOp Div) AssocLeft ]
  , [ Infix (reservedOp "+" $> BinOp Add) AssocLeft
    , Infix (reservedOp "-" $> BinOp Sub) AssocLeft ]
  , [ Infix (reservedOp "<" $> BinOp Less) AssocRight
    , Infix (reservedOp "=" $> BinOp Eql) AssocRight ]
  , [ Infix (reservedOp "||" $> BinOp Or) AssocRight ]
  ]

expr ∷ P Expr
expr = fix allExprs
  where
    allExprs p = buildExprParser table (term p)

def ∷ P Cmd
def = do
  name ← identifier
  reservedOp ":="
  t ← expr
  pure (name := t)

eval ∷ P Cmd
eval = Eval <$> expr

cmd ∷ P Cmd
cmd = try def <|> eval

parse ∷ String → Expect Cmd
parse s = case TP.runParser s cmd of
  Left (TP.ParseError e _) → parseError e
  Right c -> pure c
