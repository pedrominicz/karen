module Syntax where

type Name = String

type Var = String

type Program = [Clause]

data Clause = Clause Term [Term] deriving Show

data Term
    = Term Name [Term]
    | Var Var
    deriving Show
