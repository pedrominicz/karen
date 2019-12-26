module Karen ((===), atom, conj, disj, empty, fresh) where

-- https://github.com/seantalts/hasktrip/blob/master/doc/MicroKanren.md
-- http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

import Control.Monad.Logic
import Debug.Trace

data Term
    = Var Int
    | Term String -- [Term]
    deriving (Eq, Show)

-- fresh $ \x -> fresh $ \y -> term "Branch" [atom "Leaf", x] === term "Branch" [y, atom "Leaf"]

atom :: String -> Term
atom x = Term x

type Substitution = [(Int, Term)]

walk :: Substitution -> Term -> Term
walk s (Var v) =
    -- trace (unwords ["lookup", show v, show s]) $
    case lookup v s of
        Just x  -> x
        Nothing -> Var v
walk s x = x

unify :: Substitution -> Term -> Term -> Maybe Substitution
unify s x y = (++ s) <$> go (walk s x) (walk s y)
    where
    -- goTrace x y =
    --     trace (concat ["go (", show x, ") (", show y, ")"]) $
    --     go x y

    go x y | x == y = Just []
    go (Var v) x = Just [(v, x)]
    go x (Var v) = Just [(v, x)]
    go _ _ = Nothing

type Goal = (Substitution, Int) -> Logic (Substitution, Int)

fresh :: (Term -> Goal) -> Goal
fresh f = \(s, i) -> f (Var i) (s, i + 1)

(===) :: Term -> Term -> Goal
x === y = \(s, i) ->
    case unify s x y of
        Just s  -> return (s, i)
        Nothing -> mzero

disj :: Goal -> Goal -> Goal
disj f g = \x -> interleave (f x) (g x)

conj :: Goal -> Goal -> Goal
conj f g = \x -> f x >>- g

empty :: (Substitution, Int)
empty = ([], 0)

-- zero = atom "0"
-- one = atom "1"
-- two = atom "2"
-- observeAll $ conj (fresh $ \x -> x === zero) (fresh $ \y -> disj (y === one) (y === two)) empty
-- zeros x = disj (x === zero) (zeros x)
-- ones x = disj (x === one) (ones x)
-- observeMany 4 $ fresh (\x -> disj (zeros x) (ones x)) empty
-- a = atom "a"
-- b = atom "b"
-- c = atom "c"
-- magic = fresh (\x -> disj (x === a) (disj (x === b) (x === c)))
-- observeAll $ magic empty
--
-- test = fresh $ \x -> fresh $ \y -> fresh $ \z -> conj (x === y) $ conj (y === z) $ conj (z === x) (z === zero)
-- observeAll $ test empty
--
-- x === y
-- lookup 0 []
-- lookup 1 []
-- go (Var 0) (Var 1)
--
-- y === z
-- lookup 1 [(0,Var 1)]
-- lookup 2 [(0,Var 1)]
-- go (Var 1) (Var 2)
--
-- z === y
-- lookup 2 [(1,Var 2),(0,Var 1)]
-- lookup 0 [(1,Var 2),(0,Var 1)]
-- lookup 1 [(1,Var 2),(0,Var 1)]
-- lookup 2 [(1,Var 2),(0,Var 1)]
-- go (Var 2) (Var 2)
--
-- z === atom "0"
-- lookup 2 [(2,Var 2),(1,Var 2),(0,Var 1)]
-- lookup 2 [(2,Var 2),(1,Var 2),(0,Var 1)]
