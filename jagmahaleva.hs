module JagMaHaLeva where
import Haskore
import AutoComp

-- note updaters for mappings
fd d n = n d v
vol n  = n   v
v      = [Volume(80)]
lmap f l = line (map f l)


bar1 = lmap vol [ds 4 qn, ds 4 den, ds 4 sn, ds 4 qn, as 3 qn]

bar2 = lmap vol [g 4 qn, g 4 den, g 4 sn, g 4 qn, ds 4 qn]

bar3 = lmap vol [as 4 qn, as 4 den, as 4 sn, c 5 en, as 4 en, gs 4 en, g 4 en]

bar4 = lmap vol [g 4 qn, f 4 den, f 4 sn, f 4 dqn, g 4 en]

bar5 = lmap vol [gs 4 qn, gs 4 den, g 4 sn,f 4 qn, f 4 den, f 4 sn]

bar6 = lmap vol [g 4 qn, g 4 den, f 4 sn, ds 4 qn, ds 4 den, ds 4 sn]

bar7 = lmap vol [f 4 qn, f 4 den, f 4 sn, ds 4 en, d 4 en, c 4 en, d 4 en]

bar8 = lmap vol [ds 4 qn, g 4 den, as 4 sn, ds 4 hn]

mainVoice = bar1 :+: bar2 :+: bar3 :+: bar4 :+: bar5 :+: bar6 :+: bar7 :+: bar8

chords :: [[Chord]]
chords =  [[(Ds,"Major"),(Ds,"Major")],[(Ds,"Major"),(Ds,"Major")],[(Ds,"Major"),(Ds,"Major")],[(As,"Major"),(As,"Major")],[(Gs,"Major"),(Gs,"Major")],[(Ds,"Major"),(Ds,"Major")],[(As,"Major"),(As,"Major")],[(Ds,"Major"),(Ds,"Major")]]

jagMaHaLevaBasic = (Instr "marimba" (Tempo 3 (Phrase [Dyn SF] mainVoice))) :=: (Instr "xylo" (Tempo 3 (Phrase [Dyn SF] (autoComp basic (Ef, "Major") chords))))
jagMaHaLevaCalypso = (Instr "marimba" (Tempo 3 (Phrase [Dyn SF] mainVoice))) :=: (Instr "xylo" (Tempo 3 (Phrase [Dyn SF] (autoComp calypso (Ef, "Major") chords))))
jagMaHaLevaBoogie = (Instr "piano" (Tempo 3 (Phrase [Dyn SF] mainVoice))) :=: (Instr "xylo" (Tempo 3 (Phrase [Dyn SF] (autoComp boogie (Ef, "Major") chords))))


