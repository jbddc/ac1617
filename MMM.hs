
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

-- Parallel:
times :: (Monad fF, Strong fF) =>
-- input machines
         ((s, i) -> fF(s, o)) ->
         ((t, j) -> fF(t, r)) ->
-- output machine
         ((s, t), (i, j)) -> fF((s, t), (o, r))
times p q = (fmap m) . dstr . (p >< q) . m
             where m((p,q),(i,j)) = ((p,i),(q,j))

-- Conditional
condc :: (Monad ff, Functor ff) =>
-- condition
   ((a, i) -> ff (a, Bool)) ->
-- 'then' branch
   ((a, ()) -> ff (a,o)) ->
-- 'else' branch 
   ((a, ()) -> ff (a,o)) ->
-- output
   (a, i) -> ff (a,o)
-- definition
condc p m1 m2 = (either m1 m2) .! (fmap distl . (wrap p id outB))

outB True = Left()
outB False = Right()

-- wrapping 
wrap :: (Functor ff) => ((b, e) -> ff (a, c)) -> (i -> e) -> (c -> d) -> (b, i) -> ff (a, d) 
wrap p f g = fmap (id >< g) . p . (id >< f)

------------
ana :: Monad m => (s -> i -> m (s, o)) -> s -> [i] -> m [o]
ana m s [] = return []
ana m s (i:is) = do { (s',j) <- m s i ; js <- ana m s' is ; return(j:js) }
----------------------------------------------------------------------
