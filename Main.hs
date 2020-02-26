module Main where

import Lex
import Parse

import System.Console.Haskeline
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      file <- readFile file
      putStrLn $ show (parseFile =<< tokenize file)
    _ -> do
      name <- getProgName
      putStrLn $ unwords ["Usage:", name, "<file>"]
