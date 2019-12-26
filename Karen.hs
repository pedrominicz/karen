module Karen ((===), atom, conj, disj, empty, fresh, term) where

-- https://github.com/seantalts/hasktrip/blob/master/doc/MicroKanren.md
-- http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

import Control.Monad.State
import Data.IntMap

import Prelude hiding (lookup)

data Term
    = Var Int
    | Term String [Term]
    deriving (Eq, Show)

atom :: String -> Term
atom t = Term t []

term :: String -> [Term] -> Term
term = Term

type Substitution = IntMap Term

type Unify a = StateT Substitution Maybe a

bind :: Int -> Term -> Unify Term
bind v t = do
    modify $ insert v t
    return t

occurs :: Int -> Term -> Bool
occurs v (Term t ts) = any (occurs v) ts
occurs v (Var v')    = v == v'

apply :: Term -> Unify Term
apply (Term t ts) = Term t <$> traverse apply ts
apply (Var v) = do
    s <- get
    case lookup v s of
        Just t -> do
            guard $ not (occurs v t)
            t <- apply t
            bind v t
        Nothing -> return $ Var v

unify :: Substitution -> Term -> Term -> Maybe Substitution
unify s t t' = execStateT (go t t') s
    where
    go :: Term -> Term -> Unify Term
    go t t' = do
        t  <- apply t
        t' <- apply t'
        case (t, t') of
            (t, t') | t == t' -> return t
            (Var v, t) -> bind v t
            (t, Var v) -> bind v t
            (t, t') -> match t t'

    match (Term t ts) (Term t' ts') = do
        guard (t == t')
        ts <- zip ts ts'
        ts <- traverse (uncurry go) ts
        return $ Term t ts
    match t t' = go t t'

    zip [] []         = return []
    zip (x:xs) (y:ys) = ((x, y) :) <$> zip xs ys
    zip _ _           = mzero

type Goal = (Substitution, Int) -> [(Substitution, Int)]

fresh :: (Term -> Goal) -> Goal
fresh f = \(s, i) -> f (Var i) (s, i + 1)

(===) :: Term -> Term -> Goal
t === t' = \(s, i) ->
    case unify s t t' of
        Just s  -> [(s, i)]
        Nothing -> []

-- Depth-first search.
disj :: Goal -> Goal -> Goal
disj f g = \x -> f x ++ g x

conj :: Goal -> Goal -> Goal
conj f g = \x -> f x >>= g

-- zero = atom "0"
-- one = atom "1"
-- two = atom "2"
-- conj (fresh $ \x -> x === zero) (fresh $ \y -> disj (y === one) (y === two)) (empty, 0)
-- zeros x = disj (x === zero) (zeros x)
-- ones x = disj (x === one) (ones x)
-- take 4 $ fresh (\x -> disj (zeros x) (ones x)) (empty, 0)
-- a = atom "a"
-- b = atom "b"
-- c = atom "c"
-- magic = fresh (\x -> disj (x === a) (disj (x === b) (x === c)))
-- magic (empty, 0)
