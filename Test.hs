module Test where

import Lex
import Parse
import Solve hiding (fresh)
import Syntax
import Unify

import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as M

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
test3 = isNothing . eval $ do
    x <- fresh
    let a = term "a"
    x === a [x, x]
    apply x

test4 :: Bool
test4 = isNothing . eval $ do
    x <- fresh
    y <- fresh
    let a = term "a"
    let b = term "b"
    x === a [y, y]
    y === b [x]
    apply x

test5 :: Bool
test5 = (== 1) . length . flip evalStateT (Env M.empty 0 []) $
    unfreeze $ Term "a"
        [ Term "b" []
        , Var "X"
        , Term "c" [Var "X", Var "Y"]
        ]

test6 :: Bool
test6 = (== 1) . length . flip evalStateT (Env M.empty 0 []) $ do
    unfreeze $ Term "a"
        [ Term "b" []
        , Var "X"
        , Term "c" [Var "X", Var "Y"]
        , Term "c" [Term "b" [], Var "Z"]
        ]
    unfreeze $ Term "a"
        [ Var "X"
        , Var "Z"
        , Term "c" [Term "b" [], Var "Z"]
        , Term "c" [Var "Y", Var "Y"]
        ]

tests :: [Bool]
tests =
    [ test0
    , test1
    , test2
    , test3
    , test4
    , test5
    , test6
    ]

main :: IO ()
main = do
    let i = length tests
    case findIndex not tests of
        Just i  -> error $ unwords ["Test", show i, "failed."]
        Nothing -> putStrLn $ unwords ["All", show i, "tests were successful."]
