\section{Partial Encoding of Chick Corea's ``Children's Song No. 6''}
\label{chick}

{\small\begin{verbatim}

> module TwinkleStar where
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
> v1a = lmap (fd en) [c 4, c 4, g 4, g 4, a 4, a 4]
>
> v1b = lmap vol [g 4 qn]
>
> v2a = lmap (fd en) [f 4, f 4, e 4, e 4, d 4,d 4]
>
> v2b = lmap vol [c 4 qn]
>
> v3a = lmap (fd en) [g 4, g 4, f 4, f 4, e 4, e 4] 
>
> v3b = lmap vol [d 4 qn]
>
> v1 = v1a :+: v1b
>
> v2 = v2a :+: v2b
>
> v3 = v3a :+: v3b
>
> mainVoice = v1 :+: v2 :+: v3 :+: v3 :+: v1 :+: v2
> 
> --type Key = Haskore.KeyName
>
> type ChordProgression = [Key]
> 
> data BassStyle = Basic | Calypso | Boogie deriving (Show, Eq, Ord)
> 
> bassTable :: [(BassStyle, [Music])]
> bassTable = [(Basic,   (map (fd en) [g 4, g 4, f 4, f 4, e 4, e 4] ))
>             ,(Calypso, (map (fd en) [g 4, g 4, f 4, f 4, e 4, e 4] ))
>             ,(Boogie,  (map (fd en) [g 4, g 4, f 4, f 4, e 4, e 4] ))]
> 
> autoBass :: BassStyle -> Key -> ChordProgression -> Music
> autoBass _ _ _ = lmap (fd en) [g 4, g 4, f 4, f 4, e 4, e 4] 
> {-
> 
> basic = 2/4 Chord,1 :+: 2/4 Chord,5
> calypso = 1/4 Paus :+: 1/8 Chord,1 :+: 1/8 Chord,3 :+: 1/4 Paus :+: 1/8 Chord,1 :+: 1/8 Chord,3
> boogie = 1/8 Chord,1 :+:1/8 Chord,5 :+:1/8 Chord,6 :+:1/8 Chord,5 :+:1/8 Chord,1 :+:1/8 Chord,5 :+:1/8 Chord,6 :+:1/8 Chord,5 
> 
> basic = [Music] => (basicBass) [makeNote(2/4, 1), makeNote(2/4, 5)] [C, F] => [2/4 C,1,  2/4 F,5] => map (:+:) 
> 
> -}
> 
> -- putting it all together:
> twinkleStar = Instr "piano" (Tempo 3 (Phrase [Dyn SF] bassLine :=: mainVoice))

\end{verbatim}}

