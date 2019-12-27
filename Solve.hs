module Solve where

import Karen
import Syntax

import Control.Monad.State
import qualified Data.Map as M

type Solution = M.Map String Term

type LocalEnv = M.Map String UTerm

data Env = Env
    { env   :: LocalEnv
    , next  :: Int
    , rules :: [Clause]
    }

type Solve a = StateT Env [] a

fresh :: Solve UTerm
fresh = do
    v <- gets next
    modify $ \s -> s { next = v + 1 }
    return $ UVar v

freeze :: UTerm -> Term
freeze (UTerm t ts) = Term t (map freeze ts)
freeze (UVar v)     = Var "_"

unfreeze :: Term -> Solve UTerm
unfreeze (Term t ts) = UTerm t <$> traverse unfreeze ts
unfreeze (Var "_")   = fresh
unfreeze (Var v) = do
    t <- gets (M.lookup v . env)
    case t of
        Just t -> return t
        Nothing -> do
            t <- fresh
            modify $ \s -> s { env = M.insert v t (env s) }
            return t

rename :: Clause -> Solve (UTerm, [UTerm])
rename (Clause t ts) = do
    modify $ \s -> s { env = M.empty }
    t  <- unfreeze t
    ts <- traverse unfreeze ts
    return (t, ts)

find :: Substitution -> [UTerm] -> Solve Substitution
find s (t:ts) = do
    (s, ts) <- branch s ts
    find s ts
find s [] = return s

branch :: Substitution -> [UTerm] -> Solve (Substitution, [UTerm])
branch s (t:ts) = do
    rules <- gets rules
    rule  <- lift rules
    (t', ts') <- rename rule
    case unify s t t' of
        Just s  -> return (s, ts' ++ ts)
        Nothing -> mzero
branch s [] = return (s, [])

freezeEnv :: Substitution -> LocalEnv -> Solve Solution
freezeEnv s env =
    maybe mzero return . flip evalStateT s $ do
        env <- traverse apply env
        let env = flip M.mapWithKey env $ \v t ->
                case t of
                    UVar _ -> UTerm v []
                    t      -> t
        env <- traverse apply env
        return $ M.map freeze env


solve :: [Clause] -> [Term] -> [Solution]
solve rules ts =
    flip evalStateT (Env M.empty 0 rules) $ do
        ts  <- traverse unfreeze ts
        env <- gets env
        s   <- find empty ts
        freezeEnv s env
