module Token where

import Control.Alt ((<|>))
import Text.Parsing.Parser.String (char, oneOf)
import Text.Parsing.Parser.Token (GenLanguageDef(LanguageDef), LanguageDef, TokenParser, alphaNum, letter, makeTokenParser)

languageDef :: LanguageDef
languageDef = LanguageDef
  { commentStart: ""
  , commentEnd: ""
  , commentLine: "#"
  , nestedComments: false
  , identStart: letter
  , identLetter: alphaNum <|> char '\''
  , opStart: oneOf ['-', ':', '+', '*', '/', '|', '=', '~', '<']
  , opLetter: oneOf ['=', '|']
  , reservedNames: ["true", "false"]
  , reservedOpNames: [":=", "+", "-", "*", "/", "||", "=", "~", "<"]
  , caseSensitive: true
}

token ∷ TokenParser
token = makeTokenParser languageDef
