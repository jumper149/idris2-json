module Data.JSON.FromString

import Data.JSON.Value
import Data.Scientific

import Text.Lexer
import Text.Lexer.Core
import Text.Parser
import Text.Parser.Core

private
strNull : String
strNull = "null"

private
strTrue : String
strTrue = "true"

private
strFalse : String
strFalse = "false"

private
data Bracket = Open
             | Close

private
Eq Bracket where
  Open == Open = True
  Close == Close = True
  _ == _ = False

private
data JSONTokenKind = TNull
                   | TBool
                   | TNumber
                   | TString
                   | TSquareBracket Bracket
                   | TCurlyBracket Bracket
                   | TComma
                   | TColon
                   | TWhitespace

private
Eq JSONTokenKind where
  TNull == TNull = True
  TBool == TBool = True
  TNumber == TNumber = True
  TString == TString = True
  TSquareBracket x == TSquareBracket y = x == y
  TCurlyBracket x == TCurlyBracket y = x == y
  TComma == TComma = True
  TColon == TColon = True
  TWhitespace == TWhitespace = True
  _ == _ = False

private
TokenKind JSONTokenKind where
  TokType TNull = ()
  TokType TBool = Bool
  TokType TNumber = Scientific 10
  TokType TString = String
  TokType (TSquareBracket _) = ()
  TokType (TCurlyBracket _) = ()
  TokType TComma = ()
  TokType TColon = ()
  TokType TWhitespace = ()
  tokValue TNull _ = ()
  tokValue TBool x = x == strTrue
  tokValue TNumber x = ?parseScientific x
  tokValue TString x = ?parseString x
  tokValue (TSquareBracket _) _ = ()
  tokValue (TCurlyBracket _) _ = ()
  tokValue TComma _ = ()
  tokValue TColon _ = ()
  tokValue TWhitespace _ = ()

private
jsonTokenMap : TokenMap (Token JSONTokenKind)
jsonTokenMap = toTokenMap $
  [ (spaces, TWhitespace)
  , (is ',', TComma)
  , (is ':', TColon)
  , (is '[', TSquareBracket Open)
  , (is ']', TSquareBracket Close)
  , (is '{', TCurlyBracket Open)
  , (is '}', TCurlyBracket Close)
  , (exact strNull, TNull)
  , (exact strTrue <|> exact strFalse, TBool)
  , (?number, TNumber)
  , (?string, TString)
  ]

public export
fromString : String -> JSONValue -- TODO
