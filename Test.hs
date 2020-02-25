module Test where

import Data.List

tests :: [Bool]
tests = []

main :: IO ()
main = do
    let i = length tests
    case findIndex not tests of
        Just i  -> putStrLn $ unwords ["Test", show i, "failed."]
        Nothing -> putStrLn "All tests were successful."
