{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( repl
    ) where

import Text.Parsec
import Text.Parsec.String
import Str
import System.IO (hFlush, stdout)
import System.Console.Haskeline
import Text.ParserCombinators.Parsec.Error

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
repl = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = do
           minput <- getInputLine "|- "
           case minput of
               Nothing -> return ()
               Just input -> do
                 case parse statement "(stdin)" input of
                   Right st -> do outputStrLn . prettyPrint $ st
                                  outputStrLn $ " = " ++ (show (eval st))
                   Left e -> do outputStrLn "Error parsing input:"
                                outputStrLn . concatMap messageString . errorMessages $ e
                 loop
