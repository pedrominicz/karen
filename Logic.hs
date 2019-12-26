module Logic where

import Data.Bifunctor

newtype LogicT m a = LogicT { unLogicT :: m (Maybe (a, LogicT m a)) }

instance Functor m => Functor (LogicT m) where
    fmap f = LogicT . fmap (fmap (bimap f (fmap f))) . unLogicT

instance Applicative m => Applicative (LogicT m) where
    pure x = LogicT $ pure (Just (x, LogicT (pure Nothing)))

    {-
    LogicT f <*> x = LogicT $ (<*>) <$> g <*> unLogicT x
        where
        --g :: m (Maybe ((a, LogicT m a) -> (b, LogicT m b)))
        g = fmap (fmap (bimap f _)) f
    -}

    LogicT f <*> x = LogicT $ _

    {-
    (<*>) (LogicT f) = (<*>) <$> LogicT $
        \x -> case x of
            Just (x, xs) -> _
            Nothing      -> Nothing
    -}

instance Monad m => Monad (LogicT m) where
    LogicT x >>= f = undefined

observe :: Monad m => LogicT m a -> m [a]
observe x = do
    x <- unLogicT x
    case x of
        Just (x, xs) -> do
            xs <- observe xs
            return $ x : xs
        Nothing -> return []
