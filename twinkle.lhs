\section{Partial Encoding of Chick Corea's ``Children's Song No. 6''}
\label{chick}

{\small\begin{verbatim}

> module TwinkleStar where
> import Haskore
> import Data.Maybe
> import Data.List
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
> mapNote :: PitchClass -> Octave -> Dur -> [NoteAttribute] -> Music
> mapNote n = fromJust $ lookupNote n
> 
> lookupNote :: PitchClass -> Maybe (Octave -> Dur -> [NoteAttribute] -> Music)
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
> bassTable :: [(BassStyle, (PitchClass -> [PitchClass] -> Music))]
> bassTable = [(Basic,   basicBass)
>             ,(Calypso, calypsoBass)
>             ,(Boogie,  boogieBass)]
>             
> autoBass :: BassStyle -> PitchClass -> [[PitchClass]] -> Music
> autoBass style key chords = foldr1 (:+:) $ map (bassLookup style key) chords
>   where bassLookup style key = (fromJust $ lookup style bassTable) key
> 
> mkChord scale chord = map (getNoteFromScale scale chord) [1, 3, 5]
> 
> tuplify [x,y,z] = (x,y,z)
> 
> permuts = [(4,4,4),(4,4,5),(4,5,4),(4,5,5),(5,4,4),(5,4,5),(5,5,4),(5,5,5)]
>
> mkAllPossibleChords key scale =  map (\x -> mkPermuts (tuplify x) permuts) $ permutations $ mkChord key scale
> 
> mkPermuts _ [] = []
> mkPermuts (x,y,z) ((a,b,c):as) = ((x,a),(y,b),(z,c)) : mkPermuts (x,y,z) as
>
> {-
> 
> (C,4) (G,4) => |0-7| = 7
> (G,4) (G,4) => |7-7| = 0
> (B,5) (C,4) => |23-0| = 23
> 
> [ ((C,4), 0)
> , ((D,4), 2))]
> 
> 
> -}
> 
> lookupDistance :: (PitchClass, Integer) -> [(([PitchClass], Integer), Integer)] -> Maybe Integer
> lookupDistance (_,_) [] = Nothing
> lookupDistance (note, octave) (x:xs) = if note `elem` (getKeys x) && octave == (getOctave x)
>                                       then Just (snd x)
>                                       else lookupDistance (note,octave) xs
>   where getKeys x = fst $ fst x
>         getOctave x = snd $ fst x
> 
> getPosition key note octave = lookupDistance (note, octave) noteDistances
>   where genScale key  = concat $ take 2 $ repeat $ makeScale key
>         values = [0,2,4,5,7,9,11,12,14,16,17,19,21,23]
>         noteDistances = zipWith (\tuple value -> (tuple, value)) (zipWith (\x y -> (x, y)) (genScale key) $ (take 7 $ repeat 4) ++ (take 7 $ repeat 5)) values
>         
> getDistance key (n1,o1) (n2,o2) = abs((fromJust (getPosition key n1 o1)) - (fromJust (getPosition key n2 o2)))
> 
> toList (x,y,z) = [x,y,z]
> listOfTriplets key chord = concat $ mkAllPossibleChords key chord
> triplets key chord = map toList (listOfTriplets key chord)
> 
> costOfChords key prevChord chord = zipWith (\newChord value -> (newChord, value))
>                                    (listOfTriplets key chord)
>                                    $ map (\listOfValues -> foldr1 (+) listOfValues)
>                                    $ map (\currentChord -> zipWith (getDistance key) (toList prevChord) currentChord)
>                                    $ triplets key chord
> 
> closestChord [] (y,cost) = y
> closestChord (x:xs) (y,cost) = if cost < (snd x)
>                                then closestChord xs (y,cost)
>                                else closestChord xs x
> 
> getChord key prevChord chord = closestChord (listOfChords key prevChord chord) ((listOfChords key prevChord chord) !! 0)
>   where listOfChords k pc c = (costOfChords k pc c)
> 
> 
> --mkVoiceChord key prevChord chord = getChord key prevChord chord
> --makeVoicing key prevChord (x:xs) =
> --  (mkVoiceChord key prevChord xs) :+:  
>   
> 
> 
> --foldPrev key (x:xs) prevX  =
> --  closestChord 
> --  foldPrev key xs x 
>                                     
> -- \key -> map (\x -> zipWith (getDistance key) (toList ((C,4),(E,4),(G,5))) x) triplets
> 
> -- putting it all together:
> twinkleStar = Instr "piano" (Tempo 3 (Phrase [Dyn SF] bassLine :=: mainVoice))

\end{verbatim}}

