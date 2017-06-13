{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( repl
    ) where

import Text.Parsec
import Text.Parsec.String
import Str
import System.IO (hFlush, stdout)

data Formula a = Const Bool

statement :: GenParser Char st (Formula String)
statement = constTrue <|> constFalse

constTrue :: GenParser Char st (Formula String)
constTrue =
  do string "True"
     return (Const True)

constFalse :: GenParser Char st (Formula String)
constFalse =
  do string "False"
     return (Const False)

prettyPrint :: Formula a -> String
prettyPrint (Const value) = show value

eval :: Formula a -> Bool
eval (Const value) = value

repl :: IO ()
repl =
  do putStr "|- "
     hFlush stdout
     c <- getLine
     case parse statement "(stdin)" c of
       Right st -> do putStrLn . prettyPrint $ st
                      putStrLn $ " = " ++ (show (eval st))
                      repl
       Left e -> do putStrLn "Error parsing input:"
                    print e
                    repl
