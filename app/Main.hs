module Main where

import System.IO (hFlush, stdout)
import System.Console.Haskeline
import Text.Megaparsec

import Lib

main :: IO ()
main = runInputT defaultSettings loop
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
                         outputStrLn . indent $ parseErrorPretty e
          loop

prompt :: String
prompt = "\x22A2 " -- turnstile

indent :: String -> String
indent "" = ""
indent str = "  " ++ indent' str

indent' :: String -> String
indent' "" = ""
indent' (ch1:ch2:str)
  | ch1 == '\n' = "\n  " ++ indent' (ch2:str)
  | otherwise = ch1 : indent' (ch2:str)
indent' str = str