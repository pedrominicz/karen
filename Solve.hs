module Solve where

import Karen
import Syntax

import Control.Monad.State
import qualified Data.Map as M

type Solution = M.Map String Term

data Env = Env
    { env   :: M.Map String UTerm
    , next  :: Int
    , rules :: [Clause]
    } deriving Show

type Solve a = StateT Env [] a

fresh :: Solve UTerm
fresh = do
    v <- gets next
    modify $ \s -> s { next = v + 1 }
    return $ UVar v

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
find s (goal:goals) = do
    rules <- gets rules
    rules <- traverse rename rules
    undefined
find s [] = return s

prove :: [Clause] -> [UTerm] -> Solve Substitution
prove rules goals = evalState go (Env M.empty 0 rules)
    where
    go = undefined
