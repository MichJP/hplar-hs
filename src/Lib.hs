{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( repl
    ) where


--import Text.Parsec
--import Text.Parsec.String

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L
import Control.Applicative hiding (Const)

import Str
import System.IO (hFlush, stdout)
import System.Console.Haskeline
--import Text.ParserCombinators.Parsec.Error

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

repl :: IO ()
repl = runInputT defaultSettings loop
   where
     loop :: InputT IO ()
     loop = do
       minput <- getInputLine prompt
       case minput of
         Nothing -> return ()
         Just "" -> loop
         Just input -> do
           case parse statement "(stdin)" input of
             Right st -> do outputStrLn $ prettyPrint st
                            outputStrLn $ " = " ++ (show (eval st))
             Left e -> do outputStrLn "Error parsing input:"
                          outputStrLn . indent $ show e
           loop

prompt :: String
prompt = "|- "

indent :: String -> String
indent "" = ""
indent str = '\t' : indent' str

indent' :: String -> String
indent' "" = ""
indent' (ch1:ch2:str)
  | ch1 == '\n' = "\n\t" ++ indent' (ch2:str)
  | otherwise = ch1 : indent' (ch2:str)
indent' str = str