\section{AutoComp, a music generator written in Haskell with Haskore}
\label{chick}

{\small\begin{verbatim}

> module AutoComp where
> import Haskore
> import Data.Maybe
> import Data.List

> type ChordProgression = [Key]
> 
> type Note = PitchClass
> 
> type Octave = Integer
> 
> type AbsNote = (Note, AutoComp.Octave)
>
> type Cost = Integer
> 
> type Position = Int
> 
> type TriadChord = (AbsNote, AbsNote, AbsNote)
> 
> data BassStyle = Basic | Calypso | Boogie deriving (Show, Eq, Ord)
> 
> volume = [Volume(80)]
> 
> notes :: [[Note]]
> notes =  concat $ replicate 3 $ [[C,Bs],[Cs,Df],[D],[Ds,Ef],[E,Ff],[Es,F],[Fs,Gf],[G],[Gs,Af],[A],[As,Bf],[B,Cf]]
>
> mapNote :: Note -> Haskore.Octave -> Dur -> [NoteAttribute] -> Music
> mapNote n = fromJust $ lookupNote n
> 
> lookupNote :: Note -> Maybe (Haskore.Octave -> Dur -> [NoteAttribute] -> Music)
> lookupNote n = lookup n
>                [(Cf,cf)
>                ,(C, c )
>                ,(Cs,cs)
>                ,(Df,df)
>                ,(D, d )
>                ,(Ds,ds)
>                ,(Ef,ef)
>                ,(E, e )
>                ,(Es,es)
>                ,(Ff,ff)
>                ,(F, f )
>                ,(Fs,fs)
>                ,(Gf,gf)
>                ,(G, g )
>                ,(Gs,gs)
>                ,(Af,af)
>                ,(A, a )
>                ,(As,as)
>                ,(Bf,bf)
>                ,(B, b )
>                ,(Bs,bs)]
>            
> findPos :: [[Note]] -> Note -> Position
> findPos list elt = head $ map fst $ filter ((elt `elem`) . snd) $ zip [0..] list
> 
> majorScale :: [Position]
> majorScale = [0,2,4,5,7,9,11]
>  
> makeScale :: Note -> [[Note]]
> makeScale note = map (notes !! ) $ map ((findPos notes note) + ) majorScale
> 
> startPositionInScale :: Note -> Note -> Position
> startPositionInScale scale chord = findPos (concat $ replicate 3 $ makeScale scale) chord
> 
> makeChordScale :: Note -> Note -> [[Note]]
> makeChordScale scale chord = drop (startPositionInScale scale chord) $ concat $ replicate 3 $ makeScale scale
> 
> getNoteFromScale :: Note -> Note -> Position -> Note
> getNoteFromScale scale chord pos = head $ (makeChordScale scale chord) !! (pos-1)
> 
> mkBass :: Note -> Note -> Position -> Dur -> Music
> mkBass scale chord pos len  = (mapNote (getNoteFromScale scale chord pos)) 3 len volume
> 
> basicBass :: Note -> [Note] -> Music
> basicBass scale chords = foldr1 (:+:) $ zipBass scale chords
>   where zipBass scale [x,y] = if x == y
>                               then zipWith (\chord pos -> mkBass scale chord pos hn) chords [1,5]
>                               else zipWith (\chord pos -> mkBass scale chord pos hn) chords [1,1]
>                                 
> 
> calypsoBass :: Note -> [Note] -> Music
> calypsoBass scale chords = foldr1 (:+:) (map (\chord -> qnr :+: foldr1 (:+:) (zipCalypso scale chord)) chords)
>   where zipCalypso scale chord = map (\pos -> mkBass scale chord pos en) [1,3]
> 
> boogieBass :: Note -> [Note] -> Music
> boogieBass scale chords = foldr1 (:+:) (concat $ map (\chord -> map (\pos -> mkBass scale chord pos en) [1,5,6,5]) chords)
>   
> bassTable :: [(BassStyle, (Note -> [Note] -> Music))]
> bassTable = [(Basic,   basicBass)
>             ,(Calypso, calypsoBass)
>             ,(Boogie,  boogieBass)]
>             
> autoBass :: BassStyle -> Note -> [[Note]] -> Music
> autoBass style key chords = foldr1 (:+:) $ map (bassLookup style key) chords
>   where bassLookup style key = (fromJust $ lookup style bassTable) key
> 
> mkChord :: Note -> Note -> [Note]
> mkChord scale chord = map (getNoteFromScale scale chord) [1, 3, 5]
>
> toList :: (t, t, t) -> [t]
> toList (x,y,z) = [x,y,z]
> 
> tuplify3 :: [a] -> (a, a, a)
> tuplify3 [x,y,z] = (x,y,z)
> 
> permuts :: [(AutoComp.Octave, AutoComp.Octave, AutoComp.Octave)]
> permuts = [(4,4,4),(4,4,5),(4,5,4),(4,5,5),(5,4,4),(5,4,5),(5,5,4),(5,5,5)]
>
> mkAllPossibleChords  :: Note -> Note -> [[(AbsNote, AbsNote, AbsNote)]]
> mkAllPossibleChords key scale =  map (\x -> mkPermuts (tuplify3 x) permuts) $ permutations $ mkChord key scale
> 
> mkPermuts _ [] = []
> mkPermuts (x,y,z) ((a,b,c):as) = ((x,a),(y,b),(z,c)) : mkPermuts (x,y,z) as
> 
> 
> lookupDistance :: AbsNote -> [(([Note], AutoComp.Octave), Cost)] -> Maybe Cost
> lookupDistance (_,_) [] = Nothing
> lookupDistance (note, octave) (x:xs) =
>   if note `elem` (getKeys x) && octave == (getOctave x)
>   then Just (snd x)
>   else lookupDistance (note,octave) xs
>   where getKeys x = fst $ fst x
>         getOctave x = snd $ fst x
>         
> getPosition :: Note -> Note -> AutoComp.Octave -> Maybe Cost
> getPosition key note octave = lookupDistance (note, octave) noteDistances
>   where genScale key  = concat $ take 2 $ repeat $ makeScale key
>         values = [0,2,4,5,7,9,11,12,14,16,17,19,21,23]
>         noteDistances = zipWith (\tuple value -> (tuple, value)) (zipWith (\x y -> (x, y)) (genScale key) $ (take 7 $ repeat 4) ++ (take 7 $ repeat 5)) values
> 
> getDistance :: Note -> AbsNote -> AbsNote -> Cost
> getDistance key (n1,o1) (n2,o2) = abs((fromJust (getPosition key n1 o1)) - (fromJust (getPosition key n2 o2)))
> 
> 
> listOfTriplets :: Note -> Note -> [(AbsNote, AbsNote, AbsNote)]
> listOfTriplets key chord = concat $ mkAllPossibleChords key chord
> 
> triplets :: Note -> Note -> [[AbsNote]]
> triplets key chord = map toList (listOfTriplets key chord)
> 
> costOfChords :: Note -> TriadChord -> Note -> [(TriadChord, Cost)]
> costOfChords key prevChord chord = zipWith (\newChord value -> (newChord, value))
>                                    (listOfTriplets key chord)
>                                    $ map (\listOfValues -> foldr1 (+) listOfValues)
>                                    $ map (\currentChord -> zipWith (getDistance key) (toList prevChord) currentChord)
>                                    $ triplets key chord
> 
> -- Can be rewritten as a fold with a function that returns left or right depending on predicate.
> closestChord :: Ord a => [(t, a)] -> (t, a) -> t
> closestChord [] (y,cost) = y
> closestChord (x:xs) (y,cost) = if cost < (snd x)
>                                then closestChord xs (y,cost)
>                                else closestChord xs x
> 
> getChord :: Note -> TriadChord -> Note -> TriadChord
> getChord key prevChord chord = closestChord (listOfChords key prevChord chord) ((listOfChords key prevChord chord) !! 0)
>   where listOfChords k pc c = (costOfChords k pc c)
>         
> mkVoices :: Bool -> Note -> [Note] -> TriadChord -> [TriadChord]
> mkVoices _ _ [] _ = []
> mkVoices isFirst key (x:xs) prevChord = case isFirst of
>   True -> getChord key prevChord x : mkVoices False key xs (getChord key ((concat $ mkAllPossibleChords key x) !! 0) x)
>   False -> getChord key prevChord x : mkVoices False key xs prevChord
> 
> autoVoice :: Note -> [Note] -> Music
> autoVoice key chordProgression = foldr1 (:+:) $ map (\absNotes -> foldr1 (:=:) $ map chordify absNotes) $ map toList $ chords
>   where
>     chords :: [TriadChord]
>     chords = mkVoices True key chordProgression (getChord key (head $ concat $ mkAllPossibleChords key $ firstChord) $ firstChord)
>     firstChord = head chordProgression
>     chordify :: (Note, AutoComp.Octave) -> Music
>     chordify (note, octave) = mapNote note (fromIntegral octave) hn volume
>
> basic = Basic
> calypso = Calypso
> boogie = Boogie
> 
> autoComp :: BassStyle -> Note -> [[Note]] -> Music
> autoComp style key chordProgression = autoBass style key chordProgression :=: (autoVoice key $ concat $chordProgression)

\end{verbatim}}
