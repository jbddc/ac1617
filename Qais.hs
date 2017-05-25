
{-# OPTIONS_GHC -XNPlusKPatterns -XFlexibleInstances #-}

-- QAIS project

module Qais where

import Data.List
import Probability
import System.Process
import Cp
import GHC.IO.Exception
import GHC.Base hiding (foldr)

instance Strong Dist

-- choice between sharp f and g
schoice :: ProbRep -> (t -> a) -> (t -> a) -> t -> Dist a
schoice p f g x = choose p (f x) (g x)

-- choice between probabilistic f and g
choice :: ProbRep -> (t -> Dist a) -> (t -> Dist a) -> t -> Dist a
choice p f g x = Probability.cond (choose p True False) (f x) (g x)

-- Support of distribution:

support :: Eq a => Dist a -> [a]
support = nub . (map fst) . unD

--Size:

size :: Eq a => Dist a -> Int
size = length . support

--Compressing (for efficiency):

compress d = D [ (a,foldr (+) 0 (sel a (unD d))) | a <- support d] where
  sel :: Eq a => a -> [(a,b)] -> [b]
  sel a x = [ b | (a',b) <- x , a'==a ] 

-- pairing:
-- split
(f `kr` g) a = do { b <- f a ;
                    c <- g a ;
                    return (b,c)
                }
--fst
mfst d = do { (b,c) <- d ;
              return b
            }

--snd
msnd d = do { (b,c) <- d ;
             return c
            }

-- Monadic for

mfor :: (Integral n, Monad m) => (a -> m a) -> m a -> n -> m a
mfor b i 0 = i
mfor b i (n+1) = do {x <- mfor b i n ; b x}

-- Monadic foldr

mfoldr :: Monad m => (a -> b -> m b) -> m b -> [a] -> m b
mfoldr f u [] = u
mfoldr f u (a:x) = do { y <- mfoldr f u x ; f a y }

-- Monadic cata (for lists)

mcata g = mfoldr (curry (g.i2)) ((g.i1) ())

-- Monadic maps

mmap1 :: Monad m => (t1 -> [a] -> m [a]) -> (t -> t1) -> [t] -> m [a]
mmap1 cons _ [] = return []
mmap1 cons f (h:t) = do {
                        nt <- mmap1 cons f t;
                        cons (f h) nt;
                 }

mmap2 :: Monad m => (t -> m a) -> [t] -> m [a]
mmap2 _ [] = return []
mmap2 f (h:t) = do {
                    nt <- mmap2 f t;
                    nh <- f h;
                    return (nh:nt);
                 }

---------------- Maybe distributes over any monad ------------------

data MbT m a = MbT { mt :: m(Maybe a) }

instance (Strong m) => Strong (MbT m)

-- instance (Show a, Show (m a)) => Show (MbT m a) where show (MbT a) = fmap show a
instance (Show a,Ord a) => Show (MbT Dist a) where show (MbT a) = show a

instance (Functor m) => Functor (MbT m) where
     fmap f = MbT . fmap(fmap f) . mt

instance (Monad m, Functor m) => Monad (MbT m) where
     return = MbT . return . return
     x >>= f = (MbT . muFM.fmap(fmap (mt.f)).mt) x

instance (Monad m) => Applicative (MbT m) where
    pure = return
    (<*>) = aap

muFM :: Monad fF => fF (Maybe (fF (Maybe a))) -> fF (Maybe a)
muFM =(fmap join).join.fmap lamb

-- LaTeX interface (showing Dist graphically)

pdfit :: (Show a, Ord a) => Dist a -> IO GHC.IO.Exception.ExitCode
pdfit d = do
             dis2file d
             system "pdflatex _"
             system "open _.pdf"

dis2file :: (Show a, Ord a) => Dist a -> IO ()
dis2file = (writeFile "_.tex") . latexDist

latexDist :: (Show a, Ord a) => Dist a -> [Char]
latexDist = article . document . math . hist

article t= "\\documentclass{article}\n" ++ t

document d = "\\begin{document}\n" ++ d ++ "\\end{document}\n"

math m = "\\[\n" ++ m ++ "\\]\n"

hist d = "\\begin{array}{ll}\n" ++ (concat $ map histp x) ++ "\\end{array}\n" 
          where x = (sort . map (id >< (round.(100*))) . unD . compress) d 
                histp(n,p) = show n ++ " & \\rule{" ++ show p ++ "mm}{3pt}\\ " ++ show p ++"\\%\\\\\n"
------------
kcomp f g a = [(y,q*p) | (x,p) <- g a, (y,q) <- f x]

xpct d p = sum [ x | (b,x) <- a, b==True] where a=unD $ do { a <- d; return(p a) }

(p .&&. q) x = p x && q x

prob delta q p = (xpct delta (q .&&. p)) / (xpct delta p)
------------

