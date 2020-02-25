module Solve where

import Syntax
import Unify

import Control.Monad.State
import Data.List.NonEmpty
import qualified Data.Map as M

data Solve = Solve
  { rules :: [Clause]
  , subst :: Subst
  , local :: M.Map String Int
  , next  :: Int
  }

unfreeze :: Int -> Clause -> NonEmpty UTerm
unfreeze next (Clause t ts) = let
    go :: Term -> State (M.Map String Int, Int) UTerm
    go (Term t ts) = UTerm t <$> traverse go ts
    go (Var v) = do
      (s, i) <- get
      case M.lookup v s of
        Just v  -> return $ UVar v
        Nothing -> do
          modify $ \(s, i) -> (M.insert v i s, i + 1)
          return $ UVar i
  in evalState (do
    t  <- go t
    ts <- traverse go ts
    return $ t :| ts
  ) (M.empty, next)
