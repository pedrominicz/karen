module Solve where

import Syntax
import Unify

import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Map as M

data Binding = Binding
  { rules :: [Clause]
  , subst :: Subst
  , local :: M.Map String Int
  , next  :: Int
  }

type Solve a = LogicT (State Binding) a

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

type Solution = M.Map String Term

solve :: [Clause] -> [Term] -> [Solution]
solve rules ts = undefined
