module Main where

import Lex
import Parse
import Solve
import Syntax

import Control.Monad
import Data.List
import System.Console.Haskeline
import System.Environment
import qualified Data.Map as M

prettyTerm :: Term -> String
prettyTerm (Term t []) = t
prettyTerm (Term t ts) =
  concat $ [t, "("] ++ intersperse ", " (map prettyTerm ts) ++ [")"]
prettyTerm (Var v) = v

prettySolution :: Solution -> String
prettySolution = concat . intersperse ", " . map item . M.toList
  where
  item (v, t) = unwords [v, "=", prettyTerm t]

repl :: [Clause] -> InputT IO ()
repl cs = do
  input <- getInputLine "?- "
  case input >>= tokenize >>= parse of
    Just ts -> do
      case solve cs ts of
        [] -> outputStrLn "no."
        ts -> case filter (not . M.null) ts of
          [] -> outputStrLn "yes."
          ts -> do
            traverse (outputStrLn . (++ ".") . prettySolution) (take 4 ts)
            when (not . null $ drop 4 ts) $ outputStrLn "..."
      repl cs
    Nothing -> return ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      file <- readFile file
      case tokenize file >>= parseFile of
        Just cs -> runInputT defaultSettings $ repl cs
        Nothing -> putStrLn $ "Invalid file: " ++ file
    _ -> do
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " <file>"
