{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Unify where

import Syntax

import Control.Applicative
import Control.Monad.State
import qualified Data.IntMap as IM

-- Unification terms. Not to be confused with syntactic terms.
data UTerm
    = UTerm String [UTerm]
    | UVar Int
    deriving (Eq, Show)

type Substitution = IM.IntMap UTerm

data Binding = Binding
    { next     :: Int
    , bindings :: Substitution
    }

newtype UnifyT m a = UnifyT (StateT Binding m a)

deriving instance Functor m => Functor (UnifyT m)
deriving instance Monad m => Applicative (UnifyT m)
deriving instance Monad m => Monad (UnifyT m)
deriving instance Monad m => MonadState Binding (UnifyT m)
deriving instance MonadPlus m => Alternative (UnifyT m)
deriving instance MonadPlus m => MonadPlus (UnifyT m)

instance MonadTrans UnifyT where
    lift = UnifyT . lift

type Unify = UnifyT Maybe

fresh :: Monad m => UnifyT m UTerm
fresh = do
    v <- next <$> get
    modify (\s -> s { next = v + 1 })
    return $ UVar v

lookupUVar :: Monad m => Int -> UnifyT m (Maybe UTerm)
lookupUVar v = gets (IM.lookup v . bindings)

bind :: Monad m => Int -> UTerm -> UnifyT m UTerm
bind v t = do
    modify (\s -> s { bindings = IM.insert v t (bindings s) })
    return t

occurs :: Int -> UTerm -> Bool
occurs v (UTerm t ts) = any (occurs v) ts
occurs v (UVar v')    = v == v'

apply :: MonadPlus m => UTerm -> UnifyT m UTerm
apply (UTerm t ts) = UTerm t <$> traverse apply ts
apply (UVar v) = do
    t <- lookupUVar v
    case t of
        Just t -> do
            guard $ not (occurs v t)
            t <- apply t
            bind v t
        Nothing -> return $ UVar v

unify :: MonadPlus m => UTerm -> UTerm -> UnifyT m UTerm
unify t t' = do
    t  <- apply t
    t' <- apply t'
    case (t, t') of
        (t, t') | t == t' -> return t
        (UVar v, t) -> bind v t
        (t, UVar v) -> bind v t
        (t, t') -> match t t'

zipExact :: Alternative f => [a] -> [b] -> f [(a, b)]
zipExact [] []         = pure []
zipExact (x:xs) (y:ys) = ((x, y) :) <$> zipExact xs ys
zipExact _ _           = empty

match :: MonadPlus m => UTerm -> UTerm -> UnifyT m UTerm
match (UTerm t ts) (UTerm t' ts') = do
    guard (t == t')
    ts <- zipExact ts ts'
    ts <- traverse (uncurry unify) ts
    return $ UTerm t ts
match t t' = unify t t'
