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

deq' = tot aux (uncurry (||) . (not . empty >< not . empty)) . p1
    where 
        aux ([], l)  = ((tail l, []), head l)
        aux (l1, l2) = ((tail l1, l2), head l1)
        deq'' = tot (split (split ((tail p1) ++ p2) []) (pop' . p1)) (isNothing . deq)

queue = sum2 enq deq'

\end{code}

\begin{code}
tests = error "TODO"
\end{code}


\begin{code}
main = putStrLn "TODO"
\end{code}

\end{document}
