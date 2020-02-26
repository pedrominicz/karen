module Unify (Subst, UTerm(..), apply, bind, empty, unify) where

import Control.Monad.State
import qualified Data.IntMap as IM

data UTerm
  = UTerm String [UTerm]
  | UVar Int
  deriving (Eq, Show)

type Subst = IM.IntMap UTerm

empty :: Subst
empty = IM.empty

-- We don't care about the state if unification fails, so use `StateT` instead
-- of `MaybeT`.
type Unify a = StateT Subst Maybe a

-- Binds a term to a variable. The term is returned as a convenience for
-- `apply` and `unify`.
bind :: Int -> UTerm -> Unify UTerm
bind v t = do
  modify $ IM.insert v t
  return t

-- The infamous occurs check.
occurs :: Int -> UTerm -> Bool
occurs v (UTerm t ts) = any (occurs v) ts
occurs v (UVar v')    = v == v'

-- Applies the current substituion to a term.
apply :: UTerm -> Unify UTerm
apply (UTerm t ts) = UTerm t <$> traverse apply ts
apply (UVar v) = do
  s <- get
  case IM.lookup v s of
    Just t -> do
      guard $ not (occurs v t)
      t <- apply t
      bind v t -- Bind the variable to the applied version of `t`.
    Nothing -> return $ UVar v

unify :: Subst -> UTerm -> UTerm -> Maybe Subst
unify s t t' = execStateT (go t t') s
  where
  go :: UTerm -> UTerm -> Unify UTerm
  go t t' = do
    t  <- apply t
    t' <- apply t'
    case (t, t') of
      -- Without this case, unifying a variable to itself would cause `apply`
      -- to diverge.
      (t, t') | t == t' -> return t
      (UVar v, t) -> bind v t
      (t, UVar v) -> bind v t
      (t, t') -> match t t'

  match (UTerm t ts) (UTerm t' ts') = do
    guard (t == t')
    ts <- zipExact ts ts'
    ts <- traverse (uncurry go) ts
    return $ UTerm t ts
  match t t' = error "Unify.match: unreachable"

  zipExact [] []         = return []
  zipExact (x:xs) (y:ys) = ((x, y) :) <$> zipExact xs ys
  zipExact _ _           = mzero
