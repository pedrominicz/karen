module Solve where

import Karen
import Syntax

import Control.Monad.State
import Data.Maybe
import qualified Data.IntMap as IM
import qualified Data.Map as M

import Prelude hiding (lookup)

type Solution = M.Map String Term

data Env = Env
    { env   :: M.Map String UTerm
    , next  :: Int
    , rules :: [Clause]
    }

type Solve a = StateT Env [] a

fresh :: Solve UTerm
fresh = do
    v <- next <$> get
    modify $ \s -> s { next = v + 1 }
    return $ UVar v

lookup :: String -> Solve UTerm
lookup "_" = fresh
lookup v = do
    t <- gets (M.lookup v . env)
    case t of
        Just t -> return t
        Nothing -> do
            t <- fresh
            modify $ \s -> s { env = M.insert v t (env s) }
            return t

freeze :: UTerm -> Term
freeze (UTerm t ts) = Term t (map freeze ts)
freeze (UVar v)     = Var "_"

unfreeze :: Term -> Solve UTerm
unfreeze (Term t ts) = UTerm t <$> traverse unfreeze ts
unfreeze (Var v)     = lookup v

rename :: Clause -> Solve (UTerm, [UTerm])
rename (Clause t ts) = do
    modify $ \s -> s { env = M.empty }
    t  <- unfreeze t
    ts <- traverse unfreeze ts
    return (t, ts)

find :: Substitution -> [UTerm] -> Solve Substitution
find s (t:ts) = do
    s <- branch s t
    find s ts
find s [] = return s

branch :: Substitution -> UTerm -> Solve Substitution
branch s t = do
    rules <- rules <$> get
    s <- flip traverse rules $ \clause -> do
        (t', ts) <- rename clause
        case unify s t t' of
            Just s  -> find s ts
            Nothing -> mzero
    lift s

freezeEnv :: Substitution -> M.Map String UTerm -> Maybe Solution
freezeEnv s env =
    flip evalStateT s $ do
        env <- traverse apply env
        let env = flip M.mapWithKey env $ \v t ->
                case t of
                    UVar _ -> UTerm v []
                    t      -> t
        env <- traverse apply env
        return $ M.map freeze env

solve :: [Clause] -> [Term] -> [Solution]
solve cs (t:ts) =
    flip evalStateT (Env M.empty 0 cs) $ do
        (t, ts) <- rename $ Clause t ts
        env <- env <$> get
        s <- find IM.empty (t:ts)
        modify $ \s -> s { env = env }
        lift $ maybeToList (freezeEnv s env)
solve cs [] = []
