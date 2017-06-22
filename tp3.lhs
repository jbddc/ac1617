\documentstyle{article}
\begin{document}

\section{}

\begin{code}
import Cp
import SMT
import MMM
import Qais
import Probability
\end{code}


\begin{code}
push = flip (:)

pop = tail

top = head

empty = (0==).length
\end{code}

\begin{code}
top' = (tot (split id top) (not.empty)) . p1

pop' = tot (split pop top) (not.empty) . p1

push' = return . (split (uncurry push) bang)
\end{code}

\begin{code}
stack = sum3 pop' top' push'
\end{code}


\begin{code}

enq = extl push'

deq = extr pop'

peek = extr top'

enq' = enq

peek' = peek

flush = split (uncurry (foldl push) . swap . (reverse >< reverse)) (const [])

flush' = return . split (Cp.cond (empty . p1) flush id) bang . p1

deq' = mult . tot (deq .! flush')  (not . uncurry (&&) . (empty >< empty) . p1)

queue = sum3 enq' deq' peek'

\end{code}

\begin{code}

skip' = return . (id >< bang)

peek'' p = MbT . schoice p peek' peek'

enq'' p = MbT . schoice p enq' skip'

deq'' p = MbT . schoice p deq' peek'

queue_p p q = sum2 (enq'' p) (deq'' q)

\end{code}

\begin{code}
tests = error "TODO"
\end{code}


\begin{code}
main = putStrLn "TODO"
\end{code}

\end{document}
