module Lib
    ( statement
    , prettyPrint
    , eval
    , Formula(..)
    , atoms
    , and'em
    , or'em
    ) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Control.Applicative hiding (Const)
import Data.Set (Set)
import qualified Data.Set as Set


data Formula a = Constant Bool
               | Atom a
               | Not (Formula a)
               | Connective Op (Formula a) (Formula a)
               deriving (Show)

data Op = And | Or | Implies | Iff
        deriving (Show)

instance Functor Formula where
  fmap _ (Constant x) = Constant x
  fmap f (Atom x) = Atom (f x)
  fmap f (Not p) = Not (fmap f p)
  fmap f (Connective op p q) = Connective op (fmap f p) (fmap f q)

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

prettyPrint :: Show a => Formula a -> String
prettyPrint (Constant value) = show value
prettyPrint (Atom x) = show x
prettyPrint (Not expr) = "not " ++ prettyPrint expr
prettyPrint (Connective And l r) = prettyPrint l ++ " and " ++ prettyPrint r
prettyPrint (Connective Or l r) = prettyPrint l ++ " or " ++ prettyPrint r
prettyPrint (Connective Implies l r) = prettyPrint l ++ " implies " ++ prettyPrint r
prettyPrint (Connective Iff l r) = prettyPrint l ++ " iff " ++ prettyPrint r

eval :: Formula a -> Bool
eval (Constant value) = value
eval (Atom _x) = error "Unbound variable "
eval (Not expr) = not . eval $ expr
eval (Connective And l r) = (eval l) && (eval r)
eval (Connective Or l r) = (eval l) || (eval r)
eval (Connective Implies l r) = if eval l then eval r else True
eval (Connective Iff l r) = eval l == eval r

atoms :: Ord a => Formula a -> Set a
atoms (Constant _) = Set.empty
atoms (Atom x) = Set.singleton x
atoms (Not p) = atoms p
atoms (Connective _ p q) = Set.union (atoms p) (atoms q)

or'em :: Formula a -> Formula a -> Formula a
or'em p q = Connective Or p q

and'em :: Formula a -> Formula a -> Formula a
and'em p q = Connective And p q