module Test where

import Unify

import Lex
import Parse

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM

atom :: String -> UTerm
atom t = UTerm t []

term :: String -> [UTerm] -> UTerm
term = UTerm

(===) :: UTerm -> UTerm -> Unify UTerm
(===) = unify

eval :: Unify a -> Maybe a
eval (UnifyT x) = evalStateT x (Binding 0 IM.empty)

test0 :: Bool
test0 = isJust . eval $ do
    x <- fresh
    let a = term "a"
    let b = atom "b"
    r <- a [x, x, x] === a [b, b, b]
    apply r

test1 :: Bool
test1 = isJust . eval $ do
    x <- fresh
    y <- fresh
    z <- fresh
    x === y
    y === z
    z === x
    apply x

test2 :: Bool
test2 = isJust . eval $ do
    x <- fresh
    y <- fresh
    let a  = term "a" [atom "b"]
    let a' = term "a" [x]
    let b  = term "b" [x, y]
    let b' = term "b" [atom "b", atom "b"]
    a === a'
    b === b'
    apply b

test3 :: Bool
test3 = not . isJust . eval $ do
    x <- fresh
    let a = term "a"
    x === a [x, x]
    apply x

test4 :: Bool
test4 = not . isJust . eval $ do
    x <- fresh
    y <- fresh
    let a = term "a"
    let b = term "b"
    x === a [y, y]
    y === b [x]
    apply x

tests :: [Bool]
tests =
    [ test0
    , test1
    , test2
    , test3
    , test4
    ]

main :: IO ()
main =
    case findIndex not tests of
        Just i  -> error $ unwords ["Test", show i, "failed."]
        Nothing -> putStrLn $ "All tests were successful."
