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

m1 = stack

m2 = stack

enq' = undefined

deq' = undefined

\end{code}


\begin{code}
tests = error "TODO"
\end{code}


\begin{code}
main = putStrLn "TODO"
\end{code}

\end{document}