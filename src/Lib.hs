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

data Formula a = Constant Bool
               | Not (Formula a)
               | Connective Op (Formula a) (Formula a)

data Op = And | Or | Implies | Iff

-- https://markkarpov.com/megaparsec/parsing-simple-imperative-language.html

statement :: Parser (Formula a)
statement = sc *> expr <* eof

expr :: Parser (Formula a)
expr = makeExprParser expr' ops
  where expr' = try (between (symbol "(") (symbol ")") expr) <|> constExpr
        ops = [ [ Prefix (rword "not" *> pure Not)]
              , [ InfixL (rword "and" *> pure (Connective And))
                ]
              , [ InfixL (rword "or" *> pure (Connective Or))
                ]
              , [ InfixR (rword "implies" *> pure (Connective Implies))
                ]
              , [ InfixR (rword "iff" *> pure (Connective Iff))
                ]
              ]

constExpr :: Parser (Formula a)
constExpr =   constTrue
          <|> constFalse

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt = empty
        blockCmnt = empty

symbol :: String -> Parser String
symbol = L.symbol sc

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

constTrue :: Parser (Formula a)
constTrue = do
  void (rword "True")
  return (Constant True)

constFalse :: Parser (Formula a)
constFalse = do
  void (rword "False")
  return (Constant False)

prettyPrint :: Formula a -> String
prettyPrint (Constant value) = show value
prettyPrint (Not expr) = "not " ++ prettyPrint expr
prettyPrint (Connective And l r) = prettyPrint l ++ " and " ++ prettyPrint r
prettyPrint (Connective Or l r) = prettyPrint l ++ " or " ++ prettyPrint r
prettyPrint (Connective Implies l r) = prettyPrint l ++ " implies " ++ prettyPrint r
prettyPrint (Connective Iff l r) = prettyPrint l ++ " iff " ++ prettyPrint r

eval :: Formula a -> Bool
eval (Constant value) = value
eval (Not expr) = not . eval $ expr
eval (Connective And l r) = (eval l) && (eval r)
eval (Connective Or l r) = (eval l) || (eval r)
eval (Connective Implies l r) = if eval l then eval r else True
eval (Connective Iff l r) = eval l == eval r
