module Solve where

import Syntax
import Unify

import Control.Monad.State
import qualified Data.Map as M

data Environment = Environment
    { rules       :: [Clause]
    , environment :: M.Map Name UTerm
    }

type M a = Binding -> Environment -> (a, Binding, Environment)

type Solve a = UnifyT (State Environment) a

lookupVar :: Var -> Solve UTerm
lookupVar "_" = fresh
lookupVar v = do
    v' <- lift $ gets (M.lookup v . environment)
    case v' of
        Just t  -> return t
        Nothing -> do
            t <- fresh
            lift $ modify $
                \s -> s { environment = M.insert v t (environment s) }
            return t

freeze :: UTerm -> Term
freeze (UTerm t ts) = Term t (map freeze ts)
freeze (UVar v)     = Var "_"

unfreeze :: Term -> Solve UTerm
unfreeze (Term t ts) = UTerm t <$> traverse unfreeze ts
unfreeze (Var v)     = lookupVar v

type Solution = M.Map Name Term

solve :: [Clause] -> [Term] -> [Solution]
solve = undefined
