
{-# OPTIONS_GHC -XNPlusKPatterns #-}

-- QAIS project

module MMM where

import Cp

seqc :: (Strong ff, Monad ff) =>
-- input machines
        ((s, i) -> ff (s, o)) ->
        ((r, o) -> ff (r, k)) ->
-- output machine
        ((s, r), i) -> ff ((s, r), k)
seqc p q = ((fmap assocl) . lstr . (id >< q) . xl) .! rstr . (p >< id) . xr

extr p = fmap xr     . rstr . (p >< id) . xr

extl q = fmap assocl . lstr . (id >< q) . assocr

xr :: ((a, b), c) -> ((a, c), b)
xr((u,u'),i) = ((u,i),u')

xl :: ((a, b), c) -> (a, (c, b))
xl((u,u'),i) = (u,(i,u'))

sum2 m1 m2 = (fmap undistr) . cozip . (m1 -|- m2) . distl

-- Ternary sum:
sum3 :: (Functor ff) =>
-- input machines
        ((s, e) -> ff (s, b)) ->
        ((s, e1) -> ff (s, c)) ->
        ((s, e2) -> ff (s, d)) ->
-- output machine
        (s, Either (Either e e1) e2) -> ff (s, Either (Either b c) d)

sum3 m1 m2 m3 = sum2 (sum2 m1 m2) m3

sum4 m1 m2 m3 m4 = sum2 (sum3 m1 m2 m3) m4

rev :: Functor ff =>
-- original MM
     (((b, a), i) -> ff ((b, a), o)) ->
-- changed MM
     ((a, b), i) -> ff ((a, b), o)
rev m = fmap(swap >< id) . m . (swap >< id)

----------------------------------------------------------------------
