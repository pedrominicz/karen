module Karen ((===), atom, conj, disj, empty, fresh) where

-- https://github.com/seantalts/hasktrip/blob/master/doc/MicroKanren.md
-- http://webyrd.net/scheme-2013/papers/HemannMuKanren2013.pdf

data Term
    = Var Int
    | Term String
    deriving (Eq, Show)

atom :: String -> Term
atom x = Term x

type Substitution = [(Int, Term)]

walk :: Substitution -> Term -> Term
walk s (Var v) =
    case lookup v s of
        Just x  -> x
        Nothing -> Var v
walk s x = x

unify :: Substitution -> Term -> Term -> Maybe Substitution
unify s x y = (++ s) <$> go (walk s x) (walk s y)
    where
    go x y | x == y = Just []
    go (Var v) x = Just [(v, x)]
    go x (Var v) = Just [(v, x)]
    go _ _ = Nothing

type Goal = (Substitution, Int) -> [(Substitution, Int)]

fresh :: (Term -> Goal) -> Goal
fresh f = \(s, i) -> f (Var i) (s, i + 1)

(===) :: Term -> Term -> Goal
x === y = \(s, i) ->
    case unify s x y of
        Just s  -> [(s, i)]
        Nothing -> []

-- Depth-first search.
disj :: Goal -> Goal -> Goal
disj f g = \x -> f x ++ g x

conj :: Goal -> Goal -> Goal
conj f g = \x -> f x >>= g

empty :: (Substitution, Int)
empty = ([], 0)

-- zero = atom "0"
-- one = atom "1"
-- two = atom "2"
-- conj (fresh $ \x -> x === zero) (fresh $ \y -> disj (y === one) (y === two)) empty
-- zeros x = disj (x === zero) (zeros x)
-- ones x = disj (x === one) (ones x)
-- take 4 $ fresh (\x -> disj (zeros x) (ones x)) empty
-- a = atom "a"
-- b = atom "b"
-- c = atom "c"
-- magic = fresh (\x -> disj (x === a) (disj (x === b) (x === c)))
-- magic empty
