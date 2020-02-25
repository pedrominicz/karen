{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Logic where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.State

newtype LogicT m a =
    LogicT { runLogicT :: forall r. (a -> m r -> m r) -> m r -> m r }

observeT :: Applicative m => LogicT m a -> m [a]
observeT x = runLogicT x (fmap . (:)) (pure [])

type Logic = LogicT Identity

logic :: (forall r. (a -> r -> r) -> r -> r) -> Logic a
logic f = LogicT $ \sk ->
    Identity .
    f (\sk' -> runIdentity . sk sk' . Identity) .
    runIdentity

runLogic :: Logic a -> (a -> r -> r) -> r -> r
runLogic x sk fk = runIdentity $ runLogicT x (fmap . sk) (Identity fk)

observe :: Logic a -> [a]
observe = runIdentity . observeT

instance Functor (LogicT m) where
    fmap f x = LogicT $ \sk fk -> runLogicT x (sk . f) fk

instance Applicative (LogicT m) where
    pure x  = LogicT $ \sk fk -> sk x fk
    f <*> x = LogicT $ \sk fk ->
        runLogicT f (\f fk -> runLogicT x (sk . f) fk) fk

instance Monad (LogicT m) where
    x >>= f = LogicT $ \sk fk ->
        runLogicT x (\x fk -> runLogicT (f x) sk fk) fk

instance Foldable Logic where
    foldr f z x = runLogic x f z

instance Traversable Logic where
    traverse f x =
        runLogic x (\x fk -> (<|>) . pure <$> f x <*> fk) (pure empty)

instance Alternative (LogicT m) where
    empty   = LogicT $ \sk fk -> fk
    x <|> y = LogicT $ \sk fk ->
        runLogicT x sk (runLogicT y sk fk)

instance MonadTrans LogicT where
    lift x = LogicT $ \sk fk -> x >>= \x -> sk x fk
