module Solve where

import Syntax
import Unify

import Control.Monad.State
import Data.List.NonEmpty
import qualified Data.Map as M

import Debug.Trace

data Binding = Binding
  -- { rules :: [Clause]
  { subst :: Subst
  , local :: M.Map String Int
  , next  :: Int
  }

type Solve a = StateT Binding [] a

unfreeze :: Term -> Solve UTerm
unfreeze (Term t ts) = UTerm t <$> traverse unfreeze ts
unfreeze (Var v) = do
  s <- gets local
  case M.lookup v s of
    Just v  -> return $ UVar v
    Nothing -> do
      v' <- gets next
      modify $ \s' -> s' { local = M.insert v v' s, next = v' + 1 }
      return $ UVar v'

{-

λ> a = Clause (Term "a" []) [Term "b" []]
λ> b = Clause (Term "b" []) []
λ> evalStateT (find [a,b] [(UTerm "a" [])]) (Binding empty M.empty 0)
[()]

-}
find :: [Clause] -> [UTerm] -> Solve ()
find cs (t:ts) = do
  ts' <- branch cs t
  find cs (ts ++ traceShowId ts')
find cs [] = return ()

branch :: [Clause] -> UTerm -> Solve [UTerm]
branch cs t = do
  -- This should be the only place were non-determinism happen.
  Clause t' ts <- lift cs
  modify $ \s -> s { local = M.empty }
  s  <- gets subst
  t' <- unfreeze t'
  case unify s t t' of
    Just s  -> traverse unfreeze ts
    Nothing -> mzero

type Solution = M.Map String Term

solve :: [Clause] -> Term -> [Solution]
solve rules ts = undefined
