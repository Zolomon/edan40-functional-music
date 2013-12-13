module March where
import Haskore
import AutoComp

-- note updaters for mappings
fd d n = n d v
vol n  = n   v
v      = [Volume(80)]
lmap f l = line (map f l)

v1 = lmap vol [g 5 qn, g 5 qn, g 5 qn, ef 5 den, bf 5 sn]

v2 = lmap vol [g 5 qn, ef 5 den, bf 5 sn, g 5 hn]



mainVoice = v1 :+: v2 

marchChords :: [[Chord]]
marchChords = [[(G, "Minor"),(G, "Minor")],[(G, "Minor"),(G, "Minor")]]

marchBasic = (Instr "violin" (Tempo 2 (Phrase [Dyn SF] mainVoice))) :=: (Instr "violin" (Tempo 2 (Phrase [Dyn SF] (autoComp basic (G,"Minor") marchChords))))

marchCalypso = (Instr "violin" (Tempo 2 (Phrase [Dyn SF] mainVoice))) :=: (Instr "violin" (Tempo 2 (Phrase [Dyn SF] (autoComp calypso (G,"Minor") marchChords))))

marchBoogie =  (Instr "violin" (Tempo 2 (Phrase [Dyn SF] mainVoice))) :=: (Instr "violin" (Tempo 2 (Phrase [Dyn SF] (autoComp boogie (G,"Minor") marchChords))))



