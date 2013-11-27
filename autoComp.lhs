EDAN40 - Functional Music
-------------------------

Date: 2013-11-27  

###Authors:

* Johan Andersson, ada10jan  
* Bengt Ericsson, ada08ber  

This is the solution for the second assignment of the course
EDAN40, Functional Programming, at LTH in Lund by the authors
above.

The assignment was to generate an accompaniment and bass line for a
melody when given a key and a chord progression. 

We call the module that contain our solution, AutoComp.

> module AutoComp where
> import Haskore
> import Data.Maybe
> import Data.List

The key specifies a set of notes, the base for the rest of the song. Chord and base notes
that are generated are relative to the main key of the song. The chord progression is a sequence of chords which can be seen as guidelines for those who play the bass line and accompaniment for the song. The chords do not force extreme rules upon the musician, which means that the musician is allowed some artistic freedom while playing his part of the song, be it the bass line or the accompaniment.  

Data types
----------

To start our venture into the beautiful world of Haskell and music, we felt the need to start off with some definitions to make our journey a more joyful ride.

To make our code more fluent to read, we create a type alias for the PitchClass [1], e.g. C, Cs, Df, D etc, and call it a "Note", scales and keys are also represented as pitch classes.

[1] http://en.wikipedia.org/wiki/Pitch_class

> type Note = PitchClass
> type Key = PitchClass
> type Scale = PitchClass
> type Chord = PitchClass
 
A progression of chords is a sequence of pitch classes, or notes, a list if we may.

> type ChordProgression = [Chord]

An octave specifies the pitch of a Note and it is represented by an integer.

> type Octave = Integer

To represent a playable note in the real world, we create a compound type of a Note and an Octave to represent an "absolute note".
 
> type AbsNote = (Note, AutoComp.Octave)

When deciding what chord to play next, music theorists have concluded that the shortest distance between two chords will realize with the most profound result. The distance in semitones between two chord triads is represented by an integer value which we chose to call Cost. 

> type Cost = Integer

When figuring out the distance mentioned above, we need to generate the proper scale for the chord which we then use as a lookup table. The position will be used to retrieve the proper note from this scale.
 
> type Position = Int

The accompaniment that we generate will be in the form of triplets of absolute notes which we call TriadChords.

> type TriadChord = (AbsNote, AbsNote, AbsNote) 

There are many different common bass styles that are played by musicians all over the world, we have chosen to allow AutoComp to generate bass lines of three of these common ones; Basic bass, Calypso bass and Boogie bass.

> data BassStyle = Basic | Calypso | Boogie deriving (Show, Eq, Ord)



Helper functions
----------------

We do not want to ruin the experience or ears of our listeners, so we set the volume to 80 %.

> volume = [Volume(80)]

We felt the need to construct lists from tuples, and vice versa, so we made some creative functions to help us out on the way.

> toList :: (t, t, t) -> [t]
> toList (x,y,z) = [x,y,z]
 
> tuplify3 :: [a] -> (a, a, a)
> tuplify3 [x,y,z] = (x,y,z)

Notes are quite interesting. Some of them can be used to represent the same pitch as another note, meaning that there is an overlap. To represent this we have a list of list of notes, the sublist will contain the notes that mutually represent the same note. 

> notes :: [[Note]]
> notes =  concat $ replicate 3 $ [[C,Bs],[Cs,Df],[D],[Ds,Ef],[E,Ff],[Es,F],[Fs,Gf],[G],[Gs,Af],[A],[As,Bf],[B,Cf]]

Haskore is a cool module! To be able to play our music, we have to convert our definition of a note to Haskore's, this is accomplished by 'mapNote' which looks up a Note and returns playable Music.

> mapNote :: Note -> Haskore.Octave -> Dur -> [NoteAttribute] -> Music
> mapNote n = fromJust $ lookupNote n

Here is the associative table that is used in the above function to get the respective Haskore note.

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

Here we actually start walking down the path which will lead us to correct note position when given a scale.

> findPos :: [[Note]] -> Note -> Position
> findPos list elt = head $ map fst $ filter ((elt `elem`) . snd) $ zip [0..] list
 
To construct a scale we use the major scale which describes the semitone intervals between the notes in the scale.
              
> makeScale :: Note -> [[Note]]
> makeScale note = map (notes !! ) $ map ((findPos notes note) + ) majorScale

> majorScale :: [Position]
> majorScale = [0,2,4,5,7,9,11]

Given a scale and a chord we can find where the chord starts in its scale.

> startPositionInScale :: Scale -> Note -> Position
> startPositionInScale scale chord = findPos (concat $ replicate 3 $ makeScale scale) chord

Playing the correct chords for the bass and accompaniment will depend on the right chord scale which we construct here with the ability to retrieve a note given a position in the chosen scale.

> makeChordScale :: Scale -> Note -> [[Note]]
> makeChordScale scale chord = drop (startPositionInScale scale chord) $ concat $ replicate 3 $ makeScale scale

> getNoteFromScale :: Scale -> Note -> Position -> Note
> getNoteFromScale scale chord pos = head $ (makeChordScale scale chord) !! (pos-1)



Bass Construction
-----------------

To construct playable music we have another utility function which creates playable music when given the length to play, the scale, the chord and its position in the chord scale. 

> makeBass :: Scale -> Chord -> Position -> Dur -> Music
> makeBass scale chord pos noteLength  = (mapNote (getNoteFromScale scale chord pos)) 3 noteLength volume

Basic bass works by playing two half notes in each bar, the chords specify which notes to play. If the chords are not the same for the whole bar, we play the first note in the chord scale for the two different chords, otherwise we play the first and the fifth notes in the same chord scale.

> basicBass :: Scale -> ChordProgression -> Music
> basicBass scale chords = foldr1 (:+:) $ zipBass scale chords
>   where zipBass scale [x,y] = if x == y
>                               then zipWith (\chord pos -> makeBass scale chord pos hn) chords [1,5]
>                               else zipWith (\chord pos -> makeBass scale chord pos hn) chords [1,1]

The Calypso bass line is similar to the basic bass line. Here we insert a quarter note pause and change the length of the two half notes to eighth notes, this is repeated twice during one bar. The two played notes are from position 1 and 3 in the scale instead of 1 and 5.
 
> calypsoBass :: Scale -> ChordProgression -> Music
> calypsoBass scale chords = foldr1 (:+:) (map (\chord -> qnr :+: foldr1 (:+:) (zipCalypso scale chord)) chords)
>   where zipCalypso scale chord = map (\pos -> makeBass scale chord pos en) [1,3]

The boogie bass will play eight eighth notes for each bar, choosing the notes with positions 1, 5, 6 and 5 from the scale for the current chord (a bar is represented by two sequential chords).

> boogieBass :: Scale -> ChordProgression -> Music
> boogieBass scale chords = foldr1 (:+:) (concat $ map (\chord -> map (\pos -> makeBass scale chord pos en) [1,5,6,5]) chords)

Here is a smart associative table that maps the different bass styles to one of the generative functions above. 

> bassTable :: [(BassStyle, (AutoComp.Key -> ChordProgression -> Music))]
> bassTable = [(Basic,   basicBass)
>             ,(Calypso, calypsoBass)
>             ,(Boogie,  boogieBass)]

We have now reached the end of our journey into the bass line. Given a specific style, a key and a set of chords, two chords per bar, we will generate the magnificent bass line.

> autoBass :: BassStyle -> AutoComp.Key -> [[Chord]] -> Music
> autoBass style key chords = foldr1 (:+:) $ map (bassLookup style key) chords
>   where bassLookup style key = (fromJust $ lookup style bassTable) key



Chord Voice Consruction
-----------------------

The construction of chord triads is hard business, here we construct a list of notes that represents the triad given our scale and chord.

> makeChord :: Scale -> Chord -> ChordProgression
> makeChord scale chord = map (getNoteFromScale scale chord) [1, 3, 5]

To find the closest chord given the previous chord we had to embark on a long quest which started off with calculating all possible inversions of a chord on a scale. To do this we created a list of all octave permutations allowed in AutoComp. Then we construct a list of all possible tone permutations taking only the allowed octaves into account.

> makeAllPossibleChords  :: AutoComp.Key -> Chord -> [[(AbsNote, AbsNote, AbsNote)]]
> makeAllPossibleChords key chord =  map (\x -> makeChordPermutations (tuplify3 x) octavePermutations) $ permutations $ makeChord key chord
>   where octavePermutations :: [(AutoComp.Octave, AutoComp.Octave, AutoComp.Octave)]
>         octavePermutations = [(4,4,4),(4,4,5),(4,5,4),(4,5,5),(5,4,4),(5,4,5),(5,5,4),(5,5,5)]
> 
>         makeChordPermutations :: (t, t2, t4) -> [(t1, t3, t5)] -> [((t, t1), (t2, t3), (t4, t5))]
>         makeChordPermutations _ [] = []
>         makeChordPermutations (x,y,z) ((a,b,c):as) = ((x,a),(y,b),(z,c)) : makeChordPermutations (x,y,z) as         

Given an absolute note and an associative table, tying a list of notes and an octave to a cost, we can lookup the distance and get a cost if the absolute note exist in the table, otherwise Nothing is returned.

> lookupDistance :: AbsNote -> [((ChordProgression, AutoComp.Octave), Cost)] -> Maybe Cost
> lookupDistance (_,_) [] = Nothing
> lookupDistance (note, octave) (x:xs) =
>   if note `elem` (getKeys x) && octave == (getOctave x)
>   then Just (snd x)
>   else lookupDistance (note,octave) xs
>   where getKeys x = fst $ fst x
>         getOctave x = snd $ fst x

Now we can get the cost for a note given a key, the note we are searching for, and the octave.

> getPosition :: AutoComp.Key -> Note -> AutoComp.Octave -> Maybe Cost
> getPosition key note octave = lookupDistance (note, octave) noteDistances
>   where genScale key  = concat $ take 2 $ repeat $ makeScale key
>         values = [0,2,4,5,7,9,11,12,14,16,17,19,21,23]
>         noteDistances = zipWith (\tuple value -> (tuple, value)) (zipWith (\x y -> (x, y)) (genScale key) $ (take 7 $ repeat 4) ++ (take 7 $ repeat 5)) values

The distance, or cost, between two notes is the distance in semitones between the two tones in the scale.

> getCost :: AutoComp.Key -> AbsNote -> AbsNote -> Cost
> getCost key (n1,o1) (n2,o2) = abs((fromJust (getPosition key n1 o1)) - (fromJust (getPosition key n2 o2)))

To calculate the cost of chords we apply the help of some helper functions, mainly to juggle our triads into different shapes and forms for conveniency.
 
> listOfTriplets :: AutoComp.Key -> Chord -> [(AbsNote, AbsNote, AbsNote)]
> listOfTriplets key chord = concat $ makeAllPossibleChords key chord
 
> triplets :: AutoComp.Key -> Chord -> [[AbsNote]]
> triplets key chord = map toList (listOfTriplets key chord)

Here we create an associative table with triad chords as keys and their cost as values.

> costOfChords :: AutoComp.Key -> TriadChord -> Chord -> [(TriadChord, Cost)]
> costOfChords key prevChord chord = zipWith (\newChord value -> (newChord, value))
>                                    (listOfTriplets key chord)
>                                    $ map (\listOfValues -> foldr1 (+) listOfValues)
>                                    $ map (\currentChord -> zipWith (getCost key) (toList prevChord) currentChord)
>                                    $ triplets key chord

The closest chord to another chord is done by searching for the least cost in the associative table created above.

> closestChord :: Ord a => [(t, a)] -> (t, a) -> t
> closestChord [] (y,cost) = y
> closestChord (x:xs) (y,cost) = if cost < (snd x)
>                                then closestChord xs (y,cost)
>                                else closestChord xs x

We summon the correct chord by calling upon the help of our minion utility functions mentioned above. 

> getChord :: AutoComp.Key -> TriadChord -> Chord -> TriadChord
> getChord key prevChord chord = closestChord (listOfChords key prevChord chord) ((listOfChords key prevChord chord) !! 0)
>   where listOfChords k pc c = (costOfChords k pc c)

Now that we are able to artistically select a beautiful chord given another, we can generate a list of chords for the whole song given a value that specifies whether we are at the beginning of the song, the song's key, and the chord progression of the song and a starting chord.

> mkVoices :: Bool -> AutoComp.Key -> ChordProgression -> TriadChord -> [TriadChord]
> mkVoices _ _ [] _ = []
> mkVoices isFirst key (x:xs) prevChord = case isFirst of
>   True -> getChord key prevChord x : mkVoices False key xs (getChord key ((concat $ makeAllPossibleChords key x) !! 0) x)
>   False -> getChord key prevChord x : mkVoices False key xs prevChord

And alas, we have reached the end of our last venture! Fully equipped, we are now able to generate philosophical music given a key and a chord progression. This is done by taking the chord progression, mapping it to lists, which maps a fold over the function that converts the absolute notes into playable music, to make the music play in parallell. Then we perform another fold that sequences the chords into the supreme accompaniment.

> autoVoice :: AutoComp.Key -> ChordProgression -> Music
> autoVoice key chordProgression = foldr1 (:+:) $ map (\absNotes -> foldr1 (:=:) $ map chordify absNotes) $ map toList $ chords
>   where
>     chords :: [TriadChord]
>     chords = mkVoices True key chordProgression (getChord key (head $ concat $ makeAllPossibleChords key $ firstChord) $ firstChord)
>     firstChord = head chordProgression
>     chordify :: (Note, AutoComp.Octave) -> Music
>     chordify (note, octave) = mapNote note (fromIntegral octave) hn volume

> basic = Basic
> calypso = Calypso
> boogie = Boogie

We lied, here we actually tie up the last knot of our journey! autoComp will generate the bass line and accompaniment for the given bass style, key and chord progression.

> autoComp :: BassStyle -> AutoComp.Key -> [[Chord]] -> Music
> autoComp style key chordProgression = autoBass style key chordProgression :=: (autoVoice key $ concat $chordProgression)

Regards,
ada10jan && ada08ber

-- http://youtu.be/MfM9gQkfwyg?t=48s 
