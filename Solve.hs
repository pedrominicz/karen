module Solve where

import Syntax
import Unify

import Control.Monad.State
import Data.List.NonEmpty
import qualified Data.Map as M

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

λ> v = Var ""
λ> a = Clause (Term "a" []) [v,v,v]
λ> b = Clause (Term "b" []) []
λ> evalStateT (branch [a,a,b,a] (UTerm "a" [])) (Binding empty M.empty 0)
[[UVar 0,UVar 0,UVar 0],[UVar 0,UVar 0,UVar 0],[UVar 0,UVar 0,UVar 0]]
λ> evalStateT (branch [a,a,b,a] (UTerm "b" [])) (Binding empty M.empty 0)
[[]]
λ> evalStateT (branch [a,a,b,a] (UTerm "c" [])) (Binding empty M.empty 0)
[]

-}
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
