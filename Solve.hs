module Solve (solve) where

import Syntax
import Unify

import Control.Monad.State
import qualified Data.Map as M

data Binding = Binding
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

freeze :: UTerm -> Term
freeze (UTerm t ts) = Term t (map freeze ts)
freeze (UVar v)     = Var ('_' : show v)

find :: [Clause] -> [UTerm] -> Solve ()
find cs (t:ts) = do
  ts' <- branch cs t
  find cs (ts ++ ts')
find cs [] = return ()

branch :: [Clause] -> UTerm -> Solve [UTerm]
branch cs t = do
  -- This should be the only place were non-determinism happen.
  Clause t' ts <- lift cs
  modify $ \s -> s { local = M.empty }
  s  <- gets subst
  t' <- unfreeze t'
  case unify s t t' of
    Just s -> do
      modify $ \s' -> s' { subst = s }
      traverse unfreeze ts
    Nothing -> mzero

type Solution = M.Map String Term

{-

λ> a = Clause (Term "a" [Term "b" []]) []
λ> solve [a] [Term "a" [Var "X"]]
[fromList [("X",Term "b" [])]]

-}
solve :: [Clause] -> [Term] -> [Solution]
solve cs ts = flip evalStateT (Binding empty M.empty 0) $ do
  ts <- traverse unfreeze ts
  s  <- gets local
  find cs ts
  s' <- gets subst
  maybe (error "Solve.solve: unreachable") return . flip evalStateT s' $ do
    s <- traverse (apply . UVar) s
    s <- traverse apply $ flip M.mapWithKey s $ \v t ->
      case t of
        UVar _ -> UTerm v []
        t      -> t
    return $ M.map freeze s
