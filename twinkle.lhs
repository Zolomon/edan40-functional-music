\section{Partial Encoding of Chick Corea's ``Children's Song No. 6''}
\label{chick}

{\small\begin{verbatim}

> module TwinkleStar where
> import Haskore
> import Data.Maybe
> 
> -- note updaters for mappings
> fd d n = n d v
> vol n  = n   v
> v      = [Volume(80)]
> lmap f l = line (map f l)
> 
> 
> -- Duration = d
> -- Note = n
> -- fd d n = n d v => fd d n = n d [Volume(80)]
> 
> -- repeat something n times
> times 1 m = m
> times n m = m :+: (times (n-1) m)
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
> notes :: [[PitchClass]]
> notes =  concat $ replicate 3 $ [[C,Bs],[Cs,Df],[D],[Ds,Ef],[E,Ff],[Es,F],[Fs,Gf],[G],[Gs,Af],[A],[As,Bf],[B,Cf]]
> 
> mapNote n = fromJust $ lookupNote n
> lookupNote n = lookup n [(Cf, cf)
>           ,(C,c)
>           ,(Cs,cs)
>           ,(Df,df)
>           ,(D,d)
>           ,(Ds,ds)
>           ,(Ef,ef)
>           ,(E,e)
>           ,(Es,es)
>           ,(Ff,ff)
>           ,(F,f)
>           ,(Fs,fs)
>           ,(Gf,gf)
>           ,(G,g)
>           ,(Gs,gs)
>           ,(Af,af)
>           ,(A,a)
>           ,(As,as)
>           ,(Bf,bf)
>           ,(Bs,bs)]
>            
> findPos :: [[PitchClass]] -> PitchClass -> Int
> findPos list elt = head $ map fst $ filter ((elt `elem`) . snd) $ zip [0..] list
> 
> majorScale :: [Int]
> majorScale = [0,2,4,5,7,9,11]
> 
> --makeScale :: PitchClass -> [PitchClass]
> --makeScale note =  map (head) $ map (notes !! ) $ map ((findPos notes note)+) [0,2,4,5,7,9,11]
> 
> makeScale note = map (notes !! ) $ map ((findPos notes note) + ) majorScale
> startPositionInScale scale chord = findPos (concat $ replicate 3 $ makeScale scale) chord

> makeChordScale scale chord = drop (startPositionInScale scale chord) $ concat $ replicate 3 $ makeScale scale

> getNoteFromScale scale chord pos = head $ (makeChordScale scale chord) !! (pos-1)
> 
> mkBass scale chord pos len  = (mapNote (getNoteFromScale scale chord pos)) 4 len v

> zipBass scale chords = zipWith (\chord pos -> mkBass scale chord pos hn) chords [1,5]

> basicBass scale chords = foldr1 (:+:) $ zipBass scale chords
> 
> zipCalypso scale chord = map (\pos -> mkBass scale chord pos en) [1,3]
> calypsoBass scale chords = foldr1 (:+:) (map (\chord -> qnr :+: foldr1 (:+:) (zipCalypso scale chord)) chords)
> 
> boogieBass scale chords = foldr1 (:+:) (concat $ map (\chord -> map (\pos -> mkBass scale chord pos en) [1,5,6,5]) chords)
> -- 
> -- qnr :+: mkBass scale chord pos en :+: mkBass scale chord pos en :+: qnr :+: mkBass scale chord pos en :+: mkBass scale chord pos en
> 
> --basicBass :: PitchClass -> [PitchClass] -> Music
> --basicBass scale chords = foldr1 (:+:) zipWith (\chord pos -> mkBass scale chord pos hn) chords [1,5]
> --basicBass :: PitchClass -> [PitchClass] -> Music
> --basicBass scale chord = foldr1 (:+:) $ map (mkBass scale chord hn) [1,5]
> --basicBass scale chords = 
>
> --mkCalypsoBass scale chord = foldr (:+:) $ map (mkBass scale chord en) [1,3]
> --calypsoBass scale chord = qnr :+: mkCalypsoBass :+: 
> --endlessPiano :: Key -> Int -> 
> --endlessPiano                                                                                   } 
>   
> bassTable :: [(BassStyle, [Music])]
> bassTable = [(Basic,   (map (fd en) [g 4, g 4, f 4, f 4, e 4, e 4] ))
>             ,(Calypso, (map (fd en) [c 4, g 4, f 4, f 4, e 4, e 4] ))
>             ,(Boogie,  (map (fd en) [g 4, g 4, f 4, f 4, e 4, e 4] ))]
> 
> findToneFromKeyAndCP :: Key -> ChordProgression -> Int -> (Dur -> [NoteAttribute] -> Music)
> findToneFromKeyAndCP _ _ _ = c 4
> 
> calypso :: Key -> ChordProgression -> Music
> calypso key cp = bar :+: bar
>   where bar = qnr :+: fd en (findToneFromKeyAndCP key cp 1)
> --calypso key cp = bar :+: bar
> --  where bar = qnr :+: fd en (chordLookup key cp 1) :+: fd en (chordLookup key cp 3)
> --        chordLookup key cp pos = findToneFromKeyAndCP key cp pos
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

