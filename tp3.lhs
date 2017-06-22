\documentclass[a4paper]{article}
\usepackage[a4paper,left=3cm,right=2cm,top=2.5cm,bottom=2.5cm]{geometry}
\usepackage{palatino}
\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue]{hyperref}
\usepackage{graphicx}
%include polycode.fmt
%include tp3.sty
\begin{document}

\section{Hello}

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
toSMT m = SMT . (curry(fmap swap . m . swap))

t'' = do { x<- toSMT(peek'' 0.8) () ; y <- toSMT(deq'' 0.9) () ; toSMT(enq'' 0.6) (x+y) ; toSMT (deq'' 0.7) ()}
\end{code}


\begin{code}
main = putStrLn "TODO"
\end{code}

\end{document}
