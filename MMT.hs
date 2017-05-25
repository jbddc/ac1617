{-# OPTIONS_GHC -XUndecidableInstances -XFlexibleInstances -XMultiParamTypeClasses #-}

module MMT where

import Cp
import SMT
import Control.Applicative

data MMT m a = MMT { mmt:: m (Maybe a) } -- deriving Show

instance (Monad m) => Monad (MMT m) where
        return = MMT . return . return
        MMT x  >>= f = MMT(
                do { m <- x;
                    case m of
                        Nothing -> return Nothing
                        Just a -> mmt (f a)
                        })

instance (Monad m) => Functor (MMT m) where
        fmap f x =  do { a <- x ; return (f a) }

instance (Functor m, Monad m) => MT MMT m where
        lift = MMT . (fmap return)
       
instance (Monad m) => Strong (MMT m)

unlift :: (Functor m) => MMT m a -> m ()
unlift = fmap (!) . mmt

m2smt :: (Monad m) => (t -> s -> Maybe (a, s)) -> t -> SMT s (MMT m) a
m2smt f a = SMT(MMT . return . f a)

instance (Monad m, Monad (MMT m)) => Applicative (MMT m) where
    pure = return
    (<*>) = aap

