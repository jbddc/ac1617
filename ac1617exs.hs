
import Cp
import SMT
import MMM
import Qais
import Probability

-- (1) Stack  example ---------------------------------------------

-- functional

push = flip (:)

pop = tail

top = head

empty = (0==).length

-- methods

top' = (tot (split id top) (not.empty)) . p1

pop' = tot (split pop top) (not.empty) . p1

push' = return . (split (uncurry push) bang)

-- stack object

stack = sum3 pop' top' push'

-- (2) Folder example:

folder = sum4 right left rd new

right = seqc pop' push'

left = rev right

rd = extl top'

new = extl push'

-- (3) Abstracting from the state ---------------------------------

-- we use SMT to build the composite type:

toSMT m = SMT . (curry(fmap swap . m . swap))

-- NB: swaps needed because SMT has the types swapped

-- threads in the slides:

t= do {toSMT pop' () ; toSMT push' 2 } 

t'= do{ toSMT pop' (); t }

-- running the threads

s0=[0]

r = runSMT t s0

r' = runSMT t' s0

-- (4) Probabilistic fault injection ---------------------------------

stack_p p q = sum2 (push'' p) (pop'' q)

pop'' p = MbT . schoice p pop' top'

top'' p = MbT . schoice p top' top'

push'' q = MbT . schoice q push' skip'

skip' = return.(id >< bang)

t'' = do { x<- toSMT(pop'' 0.8) () ; y <- toSMT(pop'' 0.9) () ; toSMT(push'' 0.6) (x+y) ; toSMT (pop'' 0.7) ()}
