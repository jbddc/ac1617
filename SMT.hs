
{-# OPTIONS_GHC -XUndecidableInstances -XMultiParamTypeClasses -XFlexibleInstances #-}

-- (c) MP-I and CP (1998/99-2016/17)

module SMT where

import Cp
import St
import Control.Applicative

-- (7.5) Monad transformers ------------------------------------------------------

class (Monad m, Monad (t m))  => MT t m where   -- monad transformer class
      lift :: m a -> t m a

-- nested lifting:

dlift :: (MT t (t1 m), MT t1 m) => m a -> t (t1 m) a
dlift = lift . lift

--

data SMT s m a = SMT { smt :: (s -> m(a, s)) }
-- NB: St s corresponds to SMT s m for m the identity monad

instance (Functor m) => Functor (SMT s m) where
     fmap f (SMT g) = SMT(fmap(f><id).g)

instance (Monad m, Functor m) => Monad (SMT s m) where
     return = SMT . (curry return)
-- PW:  return a = SMT (return . (split (const a) id))
     x       >>= f = (SMT . fmap (mult. fmap (uncurry (smt . f))) . smt) x
-- PW:  (SMT g) >>= f = SMT (\s -> do { (a,s') <- g s ; let SMT k = f a in  k s' })

-- inject ST into SMT, also called the "insert" operator

iSt :: (Monad m) => St s a -> SMT s m a 
iSt = SMT . (fmap return) . st
--  = SMT . (return .) . st

-- inject M into SMT, also called the "lift" operation

--iM x = SMT(\s -> do { a <- x ; return (a,s) })
iM :: Strong m => m a -> SMT s m a
iM = SMT . (curry(rstr))

instance (Functor m, Monad m, Strong m) => MT (SMT s) m where
        lift = iM

-- extract M from SMT, related to the "unlift" operation

xM :: (Monad m,Functor m) => SMT s m a -> s -> m a
xM x = fmap p1 . (smt x)

-- that is: xM x s = do { (a,_) <- smt x s ; return a }

runSMT :: (Monad m, Functor m) => SMT s m a -> s -> m a
runSMT x s  = (xM x) s

instance (Monad m, Monad (SMT s m), Functor (SMT s m)) => Applicative (SMT s m) where
    pure = return
    (<*>) = aap

