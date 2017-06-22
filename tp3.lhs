\documentclass[a4paper]{article}
\usepackage[a4paper,left=3cm,right=2cm,top=2.5cm,bottom=2.5cm]{geometry}
\usepackage{palatino}
\usepackage[colorlinks=true,linkcolor=blue,citecolor=blue]{hyperref}
\usepackage{graphicx}
%include polycode.fmt
%include tp3.sty

\title{
		    Arquitectura \& Cálculo
\\
		Trabalho Prático 3
\\
		MiEI --- Ano Lectivo de 2016/17
}

\author{
		\dium
\\
		Universidade do Minho
\\
\\
    A70430 João Bernardo Machado Quintas Dias da Costa
\\
    A72205 Luís Martinho de Aragão Rego da Silva
\\
    A71580 Rafael Alexandre Antunes Barbosa
\\
}


\date\mydate

\begin{document}

\maketitle

\section{Dependências}

\hspace{4mm} Inicialmente são feitos os \textit{imports} das bibliotecas fornecidas na página da disciplina,
necessárias para desenvolver o trabalho prático.

\begin{code}
import Cp
import SMT
import MMM
import Qais
import Probability
\end{code}

O grupo utilizou também algumas das funções desenvolvidas no decorrer das aulas práticas,
nomeadamente as funções que definem o funcionamento de uma \textit{stack} (baseada em listas).

\begin{code}
push = flip (:)

pop = tail

top = head

empty = (0==).length
\end{code}

Também as respectivas \textit{"totalizações"} das funções apresentadas acima foram adotadas neste projeto,
pois irão ser necessárias mais à frente.

\begin{code}
top' = (tot (split id top) (not.empty)) . p1

pop' = tot (split pop top) (not.empty) . p1

push' = return . (split (uncurry push) bang)
\end{code}

\newpage
\section{Caso de Estudo}

\hspace{4mm} O caso de estudo consiste na implementação de uma \textit{Queue} composta por duas \textit{Stacks}:
\begin{itemize}
\item{Stack de \textit{dequeue} - \textit{stack} da esquerda no \textit{State} definido pelo grupo;}
\item{Stack de \textit{enqueue} - \textit{stack} da direita no \textit{State} definido pelo grupo.}
\end{itemize}

Com esta topologia do estado da \textit{Mealy Machine} pretendida, procedemos à definição base dos métodos
de uma \textit{Queue} : \textit{Enqueue}, \textit{Dequeue} e \textit{Peek}.

\begin{code}

enq = extl push'

deq = extr pop'

peek = extr top'

\end{code}

Para permitir o comportamento de \textit{flush} dos elementos da \textit{stack} de \textit{enqueues}
para a \textit{stack} de \textit{dequeues}, desenvolveu-se a função seguinte:

\begin{code}

flush = split (uncurry (foldl push) . swap . (reverse >< reverse)) (const [])

\end{code}

De realçar que esta função faz um \textit{flush} naïve, i.e., coloca sempre os elementos da \textit{stack}
de \textit{enqueues} na \textit{stack} de \textit{dequeues} através de \textit{pops} e \textit{pushs} sucessivos.

Posteriormente, partiu-se para a extensão destas funções para terem o comportamento desejado.
Tanto a função \textit{enqueue}, como a função \textit{peek}, não sofreram quaisquer alterações
pois já tinham o comportamento desejado.

No entanto, a função \textit{flush} teve de ser extendida para apenas realizar a ação de \textit{flushing}
quando a \textit{stack} de \textit{dequeues} estiver vazia. Desta forma, o \textit{flush} não é realizado
desnecessariamente quando a \textit{stack} em questão tem elementos que podem ser retirados.

\begin{code}

enq' = enq

peek' = peek

flush' = return . split (Cp.cond (empty . p1) flush id) bang . p1

\end{code}

A função \textit{dequeue} também teve de ser extendida para representar o comportamento desejado.
A nova função desenvolvida, \textit{deq'}, retorna Nothing caso não haja elementos em nenhuma das \textit{stacks},
como é pretendido, faz \textit{flush} na situação em que tem elementos apenas na \textit{stack} de \textit{enqueues}, e
faz \textit{enqueue} de um elemento nas restantes situações.

\begin{code}

deq' = mult . tot (deq .! flush')  (not . uncurry (&&) . (empty >< empty) . p1)

\end{code}

Por fim é representada a nossa \textit{queue}, expondo os métodos que esta disponibiliza através da
função \textit{sum3} (de salientar que o flush não é exposto para o utilizador, tratando-se de uma
operação intermédia interna da máquina) :

\begin{code}

queue = sum3 enq' deq' peek'

\end{code}

\newpage

\section{Faulty \textit{Queue}}

Considerando como ponto de partida a \textit{stack probabilística} das aulas,
foi construída a \textit{faulty queue} através da introdução de probabilidades de sucesso
nos métodos que constituem a \textit{queue} apresentada acima:

\begin{code}

skip' = return . (id >< bang)

peek'' p = MbT . schoice p peek' peek'

enq'' p = MbT . schoice p enq' skip'

deq'' p = MbT . schoice p deq' peek'

queue_p p q = sum2 (enq'' p) (deq'' q)

\end{code}

\section{Testes e Resultados}

Segue-se um teste que avalia o funcionamento da \textit{Faulty Queue}, utilizando outra
função desenvolvida nas aulas, \textit{toSMT}.

\begin{code}
toSMT m = SMT . (curry(fmap swap . m . swap))

t'' = do
      x<- toSMT(peek'' 0.8) ()
      y <- toSMT(deq'' 0.9) ()
      toSMT(enq'' 0.6) (x+y)
      toSMT (deq'' 0.7) ()
\end{code}

O resultado obtido, para a execução apresentada, foi o seguinte:

\begin{spec}
$ runSMT t'' ([1,2,3],[4,5,6])

Just 2  90.0%
Just 1  10.0%

\end{spec}


Podemos concluir que, na nossa implementação pelo menos, não será possível obter dados sobre a realização
de \textit{flush} pois trata-se de um método interno da máquina.

\end{document}
