{-# LANGUAGE QuasiQuotes #-}
module Lib
    ( someFunc
    ) where

import Text.Parsec
import Text.Parsec.String
import Str

data Formula a = Const Bool

statement :: GenParser Char st (Formula String)
statement =
  do string "True"
     return (Const True)-- <|> string "False"
--     return (Const False)

prettyPrint :: Formula a -> String
prettyPrint (Const value) = show value

someFunc :: IO ()
someFunc = case (parse statement "" [str|True|]) of
  Right st -> putStrLn (prettyPrint st)
  Left _ -> putStrLn "Error"
