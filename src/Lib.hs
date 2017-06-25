{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( statement
    , prettyPrint
    , eval
    ) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Control.Applicative hiding (Const)

--import Str

data Formula a = Const Bool

statement :: Parser (Formula String)
statement = constTrue <|> constFalse

-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt = empty
        blockCmnt = empty

symbol :: String -> Parser String
symbol = L.symbol sc

constTrue :: Parser (Formula String)
constTrue = do
  void (symbol "True")
  return (Const True)

constFalse :: Parser (Formula String)
constFalse = do
  void (symbol "False")
  return (Const False)

prettyPrint :: Formula a -> String
prettyPrint (Const value) = show value

eval :: Formula a -> Bool
eval (Const value) = value
