module Syntax where

data Clause = Clause Term [Term] deriving Show

data Term
  = Term String [Term]
  | Var String
  deriving Show
