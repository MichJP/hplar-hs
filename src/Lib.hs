module Lib
    ( someFunc
    ) where

data Formula a = Const Bool

prettyPrint :: Formula a -> String
prettyPrint (Const value) = show value

someFunc :: IO ()
someFunc = putStrLn . prettyPrint $ Const True
