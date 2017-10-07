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
import qualified Data.Map.Strict as Map

data Formula = Constant Bool
             | Atom String
             | Not Formula
             | Connective Op Formula Formula
             deriving (Show)

data Op = And | Or | Implies | Iff
        deriving (Show)

statement :: Parser Formula
statement = sc *> expr <* eof

expr :: Parser Formula
expr = makeExprParser expr' ops
  where expr' = try (between (symbol "(") (symbol ")") expr) <|> constExpr <|> atomicFormula
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

constExpr :: Parser Formula
constExpr =   constTrue
          <|> constFalse

atomicFormula :: Parser Formula
atomicFormula = do
  x <- identifier
  return (Atom x)

rws :: [String] -- list of reserved words
rws = ["True", "False", "not", "and", "or", "implies", "iff"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt = empty
        blockCmnt = empty

symbol :: String -> Parser String
symbol = L.symbol sc

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

constTrue :: Parser Formula
constTrue = do
  void (rword "True")
  return (Constant True)

constFalse :: Parser Formula
constFalse = do
  void (rword "False")
  return (Constant False)

prettyPrint :: Formula -> String
prettyPrint (Constant value) = show value
prettyPrint (Atom x) = x
prettyPrint (Not expr) = "not " ++ prettyPrint expr
prettyPrint (Connective And l r) = prettyPrint l ++ " and " ++ prettyPrint r
prettyPrint (Connective Or l r) = prettyPrint l ++ " or " ++ prettyPrint r
prettyPrint (Connective Implies l r) = prettyPrint l ++ " implies " ++ prettyPrint r
prettyPrint (Connective Iff l r) = prettyPrint l ++ " iff " ++ prettyPrint r

eval :: Formula -> Bool
eval (Constant value) = value
eval (Atom x) = error ("Unbound variable " ++ x)
eval (Not expr) = not . eval $ expr
eval (Connective And l r) = (eval l) && (eval r)
eval (Connective Or l r) = (eval l) || (eval r)
eval (Connective Implies l r) = if eval l then eval r else True
eval (Connective Iff l r) = eval l == eval r

evalUnder :: Formula -> Map.Map String Bool -> Bool
evalUnder (Constant value) _ = value
evalUnder (Atom x) valuation = case Map.lookup x valuation of
                                 Just v -> v
                                 Nothing -> error ("Unbound variable " ++ x)
evalUnder (Not expr) valuation = not $ evalUnder expr valuation
evalUnder (Connective And l r) valuation = (evalUnder l valuation) && (evalUnder r valuation)
evalUnder (Connective Or l r) valuation = (evalUnder l valuation) || (evalUnder r valuation)
evalUnder (Connective Implies l r) valuation = if evalUnder l valuation then evalUnder r valuation else True
evalUnder (Connective Iff l r) valuation = evalUnder l valuation == evalUnder r valuation

atoms :: Formula -> Set String
atoms (Constant _) = Set.empty
atoms (Atom x) = Set.singleton x
atoms (Not p) = atoms p
atoms (Connective _ p q) = Set.union (atoms p) (atoms q)

or'em :: Formula -> Formula -> Formula
or'em p q = Connective Or p q

and'em :: Formula -> Formula -> Formula
and'em p q = Connective And p q
