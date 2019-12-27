module Karen where

import Control.Monad.State
import qualified Data.IntMap as IM

data UTerm
    = UVar Int
    | UTerm String [UTerm]
    deriving (Eq, Show)

type Substitution = IM.IntMap UTerm

empty :: IM.IntMap UTerm
empty = IM.empty

type Unify a = StateT Substitution Maybe a

bind :: Int -> UTerm -> Unify UTerm
bind v t = do
    modify $ IM.insert v t
    return t

occurs :: Int -> UTerm -> Bool
occurs v (UTerm t ts) = any (occurs v) ts
occurs v (UVar v')    = v == v'

apply :: UTerm -> Unify UTerm
apply (UTerm t ts) = UTerm t <$> traverse apply ts
apply (UVar v) = do
    s <- get
    case IM.lookup v s of
        Just t -> do
            guard $ not (occurs v t)
            t <- apply t
            bind v t
        Nothing -> return $ UVar v

unify :: Substitution -> UTerm -> UTerm -> Maybe Substitution
unify s t t' = execStateT (go t t') s
    where
    go :: UTerm -> UTerm -> Unify UTerm
    go t t' = do
        t  <- apply t
        t' <- apply t'
        case (t, t') of
            (t, t') | t == t' -> return t
            (UVar v, t) -> bind v t
            (t, UVar v) -> bind v t
            (t, t') -> match t t'

    match (UTerm t ts) (UTerm t' ts') = do
        guard (t == t')
        ts <- zip ts ts'
        ts <- traverse (uncurry go) ts
        return $ UTerm t ts
    match t t' = go t t'

    zip [] []         = return []
    zip (x:xs) (y:ys) = ((x, y) :) <$> zip xs ys
    zip _ _           = mzero
