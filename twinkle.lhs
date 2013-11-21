\section{Partial Encoding of Chick Corea's ``Children's Song No. 6''}
\label{chick}

{\small\begin{verbatim}

> module TwinkleStart where
> import Haskore
> 
> -- note updaters for mappings
> fd d n = n d v
> vol n  = n   v
> v      = [Volume(80)]
> lmap f l = line (map f l)
> 
> -- repeat something n times
> times 1 m = m
> times n m = m :+: (times (n-1)m)
> 
> bassLine = lmap (fd dqn) [b 3]
> 
> mainVoice = lmap (fd en) [a 5]
> 
> -- putting it all together:
> twinkleStar = Instr "piano" (Tempo 3 (Phrase [Dyn SF] bassLine :=: mainVoice))

\end{verbatim}}
