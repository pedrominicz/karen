module Test where

import Lex

import Data.List
import Data.Maybe

lexTests :: [Bool]
lexTests =
  [ fromMaybe False $ (12 ==) . length <$> tokenize "last(X,cons(X,nil))."
  , fromMaybe False $ (19 ==) . length <$> tokenize "last(X,cons(_,Xs)) :- last(X,Xs)."
  , fromMaybe False $ (17 ==) . length <$> tokenize "but_last(X,cons(X,cons(_,nil)))."
  , fromMaybe False $ (19 ==) . length <$> tokenize "but_last(X,cons(_,Xs)) :- but_last(X,Xs)."
  ]

tests :: [Bool]
tests = lexTests

main :: IO ()
main =
  case findIndex not tests of
    Just i  -> putStrLn $ unwords ["Test", show i, "failed."]
    Nothing -> putStrLn "All tests were successful."
