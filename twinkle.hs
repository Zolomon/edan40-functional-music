module TwinkleStar where
import Haskore
import AutoComp

-- note updaters for mappings
fd d n = n d v
vol n  = n   v
v      = [Volume(80)]
lmap f l = line (map f l)

v1a = lmap (fd qn) [c 4, c 4, g 4, g 4, a 4, a 4]

v1b = lmap vol [g 4 hn]

v2a = lmap (fd qn) [f 4, f 4, e 4, e 4, d 4,d 4]

v2b = lmap vol [c 4 hn]

v3a = lmap (fd qn) [g 4, g 4, f 4, f 4, e 4, e 4] 

v3b = lmap vol [d 4 hn]

v1 = v1a :+: v1b

v2 = v2a :+: v2b

v3 = v3a :+: v3b

mainVoice = v1 :+: v2 :+: v3 :+: v3 :+: v1 :+: v2

twinkleChords = [[C,C],[F,C],[G,C],[G,C],[C,G],[C,G],[C,G],[C,G],[C,C],[F,C],[G,C],[G,C]]

twinkleBasic = (Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainVoice))) :=: (Instr "xylo" (Tempo 3 (Phrase [Dyn SF] (autoComp basic C twinkleChords))))

twinkleCalypso = (Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainVoice))) :=: (Instr "xylo" (Tempo 3 (Phrase [Dyn SF] (autoComp calypso C twinkleChords))))

twinkleBoogie =  (Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainVoice))) :=: (Instr "xylo" (Tempo 3 (Phrase [Dyn SF] (autoComp boogie C twinkleChords))))



