module Error where

import Prelude
import Syntax (Lit, Name)

import Data.Either (Either(..))

type Expect a = Either Error a

data Error = ParseError String
           | TypeMismatch Lit String
           | UnknownValue Name
           | TheImpossibleHappened String

instance showError ∷ Show Error where
  show (ParseError s) = "Parse error: " <> s
  show (TypeMismatch l s) = "Type mismatch: expecting " <> s <> "but found" <> show l
  show (UnknownValue n) = "Unknown value: " <> n
  show (TheImpossibleHappened msg) = "The impossible happened: " <> msg

throw ∷ forall a. Error → Expect a
throw = Left

parseError :: forall a. String -> Expect a
parseError = throw <<< ParseError

typeMismatch :: forall a. Lit -> String -> Expect a
typeMismatch l = throw <<< TypeMismatch l

unknownValue :: forall a. Name -> Expect a
unknownValue = throw <<< UnknownValue

theImpossibleHappened :: forall a. String -> Expect a
theImpossibleHappened = throw <<< TheImpossibleHappened
