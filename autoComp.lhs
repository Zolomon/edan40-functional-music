EDAN40 - Functional Music
-------------------------

Date: 2013-11-27  

------------ Authors:

* Johan Andersson, ada10jan  
* Bengt Ericsson, ada08ber  

This is the solution for the second assignment of the course
EDAN40, Functional Programming, at LTH in Lund by the authors
above.

The assignment was to generate an accompaniment and bass line for a
melody when given a key and a chord progression. 

We call the module that contain our solution, AutoComp.

\begin{code}

module AutoComp where
import Haskore hiding (Key)
import Data.Maybe
import Data.List

\end{code}

The key specifies a set of notes, the base for the rest of the song. Chord and base notes
that are generated are relative to the main key of the song. The chord progression is a sequence 
of chords which can be seen as guidelines for those who play the bass line and accompaniment for 
the song. The chords do not force extreme rules upon the musician, which means that the musician 
is allowed some artistic freedom while playing his part of the song, be it the bass line or the 
accompaniment.  

Data types
----------

To start our venture into the beautiful world of Haskell and music, we felt the need to start off 
with some definitions to make our journey a more joyful ride.

To make our code more fluent to read, we create a type alias for the PitchClass [1], e.g. C, Cs, Df, 
D etc, and call it a "Note". The harmonic quality is represented as a string. Keys and chords are 
represented as a PitchClass and a HarmonicQuality tuple.

[1] http://en.wikipedia.org/wiki/Pitch_class

\begin{code}

type Note = PitchClass

type HarmonicQuality = String 

type Key = (PitchClass,HarmonicQuality)
type Chord = (PitchClass,HarmonicQuality)

\end{code}
 
A progression of chords is a sequence of pitch classes, or notes, a list if we may.
\begin{code}
type SimpleChord = [Note] -- Oklart vad denna representerar. Nothing to see here...

type ChordProgression = [Chord]
\end{code}

An octave specifies the pitch of a Note and it is represented by an integer.

\begin{code}
type Octave = Int
\end{code}

To represent a playable note in the real world, we create a compound type of a Note and an 
Octave to represent an "absolute note".

\begin{code}
type AbsNote = (Note, Int)
\end{code}

When deciding what chord to play next, music theorists have concluded that the shortest distance 
between two chords will realize with the most profound result. The distance in semitones between 
two chord triads is represented by an integer value which we chose to call Cost. 

\begin{code}
type Cost = Int
\end{code}

When figuring out the distance mentioned above, we need to generate the proper scale for the chord 
which we then use as a lookup table. The position will be used to retrieve the proper note from this 
scale.

\begin{code}
type Position = Int
\end{code}

The accompaniment that we generate will be in the form of triplets of absolute notes which we call 
TriadChords.

\begin{code}
type TriadChord = (AbsNote, AbsNote, AbsNote) 
\end{code}

There are many different common bass styles that are played by musicians all over the world, we have 
chosen to allow AutoComp to generate bass lines of three of these common ones; Basic bass, Calypso 
bass and Boogie bass.

\begin{code}
data BassStyle = Basic | Calypso | Boogie deriving (Show, Eq, Ord)
\end{code}


Helper functions
----------------

We do not want to ruin the experience or ears of our listeners, so we set the volume to 80 %.

\begin{code}
volume = [Volume(80)]
\end{code}

We felt the need to construct lists from tuples, and vice versa, so we made some creative 
functions to help us out on the way.

\begin{code}

toList :: (t, t, t) -> [t]
toList (x,y,z) = [x,y,z]
 
tuplify3 :: [a] -> (a, a, a)
tuplify3 [x,y,z] = (x,y,z)

\end{code}

Notes are quite interesting. Some of them can be used to represent the same pitch as another note, meaning 
that there is an overlap. To represent this we have a list of list of notes, the sublist will contain the 
notes that mutually represent the same note. 

\begin{code}

notes :: [[Note]]
notes =  concat $ replicate 3 $ [[C,Bs],[Cs,Df],[D],[Ds,Ef],[E,Ff],[Es,F],[Fs,Gf],[G],[Gs,Af],[A],[As,Bf],[B,Cf]]

\end{code}

Haskore is a cool module! To be able to play our music, we have to convert our definition of a note to 
Haskore's, this is accomplished by 'mapNote' which looks up a Note and returns playable Music.

\begin{code}

mapNote :: Note -> Haskore.Octave -> Dur -> [NoteAttribute] -> Music
mapNote n = fromJust $ lookupNote n

\end{code}
Here is the associative table that is used in the above function to get the respective Haskore note.

\begin{code}

lookupNote :: Note -> Maybe (Haskore.Octave -> Dur -> [NoteAttribute] -> Music)
lookupNote n = lookup n
                [(Cf,cf)
                ,(C, c )
                ,(Cs,cs)
                ,(Df,df)
                ,(D, d )
                ,(Ds,ds)
                ,(Ef,ef)
                ,(E, e )
                ,(Es,es)
                ,(Ff,ff)
                ,(F, f )
                ,(Fs,fs)
                ,(Gf,gf)
                ,(G, g )
                ,(Gs,gs)
                ,(Af,af)
                ,(A, a )
                ,(As,as)
                ,(Bf,bf)
                ,(B, b )
                ,(Bs,bs)]

\end{code}

Here we actually start walking down the path which will lead us to correct note position when given a scale.

\begin{code}

findPos :: [[Note]] -> Note -> Position
findPos list elt = head $ map fst $ filter ((elt `elem`) . snd) $ zip [0..] list

\end{code}

To construct a scale we perform a lookup based on the harmonic quality in the key. 

\begin{code}
makeScale :: Key -> [[Note]]
makeScale (note, hq) = 
	map (notes !! ) $ map ((findPos notes note) + ) (scaleIntervals hq) 

scaleIntervals :: HarmonicQuality -> [Position]
scaleIntervals "Ionian" 	= [0, 2, 4, 5, 7, 9, 11] -- "major"
scaleIntervals "Major"	 	= scaleIntervals "Ionian"
scaleIntervals "Dorian"		= [0, 2, 3, 5, 7, 9, 10]	
scaleIntervals "Phrygian" 	= [0, 1, 3, 5, 7, 8, 10]	
scaleIntervals "Lydian"		= [0, 2, 4, 6, 7, 9, 11]	
scaleIntervals "Mixolydian"     = [0, 2, 4, 5, 7, 9, 10]		
scaleIntervals "Aeolian" 	= [0, 2, 3, 5, 7, 8, 10] -- "minor"
scaleIntervals "Minor"	 	= scaleIntervals "Aeolian"
\end{code}

Given a scale and a chord we can find where the chord starts in its scale.

\begin{code}
startPositionInScale :: Key -> Note -> Position
startPositionInScale key note = findPos (concat $ replicate 3 $ makeScale key) note
\end{code}

Playing the correct chords for the bass and accompaniment will depend on the right chord scale which we 
construct here with the ability to retrieve a note given a position in the chosen scale.

\begin{code}
 
makeChordScale :: Key -> Note -> [[Note]]
makeChordScale key root = drop (startPositionInScale key root) $ concat $ replicate 3 $ makeScale key

getNoteFromScale :: Key -> Note -> Position -> Note
getNoteFromScale key note pos = 
	head $ (makeChordScale key note) !! (pos-1)


\end{code}


Bass Construction
-----------------

To construct playable music we have another utility function which creates playable music when given the 
length to play, the scale, the chord and its position in the chord scale. 

\begin{code}
 
makeBass :: Key -> Chord -> Position -> Dur -> Music
makeBass scale chord pos noteLength  = 
	(mapNote $ bassNote scale chord pos) 3 noteLength volume

bassNote:: Key -> Chord -> Position -> Note 
bassNote scale chord pos
	| chordInKey scale chord = 
		(getNoteFromScale scale (fst chord) pos)
	| otherwise = (getNoteFromScale chord (fst chord) pos)

\end{code}

Basic bass works by playing two half notes in each bar, the chords specify which notes to play. 
If the chords are not the same for the whole bar, we play the first note in the chord scale for 
the two different chords, otherwise we play the first and the fifth notes in the same chord scale.

\begin{code}
 
basicBass :: Key -> ChordProgression -> Music
basicBass scale chords = foldr1 (:+:) $ zipBass scale chords
  where zipBass scale [x,y] = if x == y
                              then zipWith (\chord pos -> makeBass scale chord pos hn) chords [1,5]
                              else zipWith (\chord pos -> makeBass scale chord pos hn) chords [1,1]

\end{code}

The Calypso bass line is similar to the basic bass line. Here we insert a quarter note pause and 
change the length of the two half notes to eighth notes, this is repeated twice during one bar. 
The two played notes are from position 1 and 3 in the scale instead of 1 and 5.

\begin{code}
 
calypsoBass :: Key -> ChordProgression -> Music
calypsoBass scale chords = foldr1 (:+:) (map (\chord -> qnr :+: foldr1 (:+:) (zipCalypso scale chord)) chords)
  where zipCalypso scale chord = map (\pos -> makeBass scale chord pos en) [1,3]

\end{code}

The boogie bass will play eight eighth notes for each bar, choosing the notes with positions 1, 5, 
6 and 5 from the scale for the current chord (a bar is represented by two sequential chords).

\begin{code}
 
boogieBass :: Key -> ChordProgression -> Music
boogieBass scale chords = foldr1 (:+:) (concat $ map (\chord -> map (\pos -> makeBass scale chord pos en) [1,5,6,5]) chords)

\end{code}

Here is a smart associative table that maps the different bass styles to one of the generative 
functions above. 

\begin{code}
 
bassTable :: [(BassStyle, (Key -> ChordProgression -> Music))]
bassTable = [(Basic,   basicBass)
            ,(Calypso, calypsoBass)
            ,(Boogie,  boogieBass)]

\end{code}

We have now reached the end of our journey into the bass line. Given a specific style, a key and a set 
of chords, two chords per bar, we will generate the magnificent bass line.

\begin{code}
 
autoBass :: BassStyle -> Key -> [[Chord]] -> Music
autoBass style key chords = foldr1 (:+:) $ map (bassLookup style key) chords
  where bassLookup style key = (fromJust $ lookup style bassTable) key

\end{code}


Chord Voice Consruction
-----------------------
The construction of chord triads is hard business, here we construct a list of notes that represents the 
triad given our scale and chord. First we check if the chord is in the current key. If it isn't, then we 
temporarily change the key of the song to that of the chord.

\begin{code}

makeChord :: Key -> Chord -> SimpleChord
makeChord scale chord 
	| chordInKey scale  chord = 
		map (getNoteFromScale scale $ fst chord) [1, 3, 5]
	| otherwise = map (getNoteFromScale chord $ fst chord) [1, 3, 5]

\end{code}

A chord triad is in key if the root is in the scale, and the third of the chord matches that of the scale 
at that position.

\begin{code}

chordInKey :: Key -> Chord -> Bool
chordInKey key (chord, chq) = 
	(noteInKey key chord) && (hasCorrectThird (snd key) pos chq) 
		where pos = startPositionInScale key chord
			  
noteInKey ::  Key -> Note -> Bool
noteInKey key note = note `elem` (concat $ makeScale key)

\end{code}
If we select a root note anywhere in a scale, and then move two positions up in the scale, we reach the 
third. There are major and minor thirds. The function "hasCorrectThird" takes a scale, a position in that 
scale and a HarmonicQuality and checks if the thirds match.
\begin{code}

-- Kollar om ackordets ters stämmer med skalan
hasCorrectThird :: HarmonicQuality -> Position -> HarmonicQuality -> Bool
hasCorrectThird "Ionian" pos chordHQ = 
	(["Major", "Minor", "Minor", "Major", "Major", "Minor", "Minor"] !! pos) == chordHQ

hasCorrectThird "Major" pos chordHQ = hasCorrectThird "Ionian" pos chordHQ
 
hasCorrectThird "Aeolian" pos chordHQ = 
	(["Minor", "Minor", "Major", "Minor", "Minor", "Major", "Major"] !! pos) == chordHQ

hasCorrectThird "Minor" pos chordHQ = hasCorrectThird "Aeolian" pos chordHQ

hasCorrectThird _ _ _= False


\end{code}

To find the closest chord given the previous chord we had to embark on a long quest which started off 
with calculating all possible inversions of a chord on a scale. To do this we created a list of all 
octave permutations allowed in AutoComp. Then we construct a list of all possible tone permutations 
taking only the allowed octaves into account.

\begin{code}
 
makeAllPossibleChords  :: Key -> Chord -> [[(AbsNote, AbsNote, AbsNote)]]
makeAllPossibleChords key chord =  map (\x -> makeChordPermutations (tuplify3 x) octavePermutations) $ permutations $ makeChord key chord
  where octavePermutations :: [(AutoComp.Octave, AutoComp.Octave, AutoComp.Octave)]
        octavePermutations = [(4,4,4),(4,4,5),(4,5,4),(4,5,5),(5,4,4),(5,4,5),(5,5,4),(5,5,5)]
 
        makeChordPermutations :: (t, t2, t4) -> [(t1, t3, t5)] -> [((t, t1), (t2, t3), (t4, t5))]
        makeChordPermutations _ [] = []
        makeChordPermutations (x,y,z) ((a,b,c):as) = ((x,a),(y,b),(z,c)) : makeChordPermutations (x,y,z) as         

\end{code}

Given an absolute note and an associative table, tying a list of notes and an octave to a cost, we can 
lookup the distance and get a cost if the absolute note exist in the table, otherwise Nothing is returned.

\begin{code}
 
lookupDistance :: AbsNote -> [((SimpleChord, AutoComp.Octave), Cost)] -> Maybe Cost
lookupDistance (_,_) [] = Nothing
lookupDistance (note, octave) (x:xs) =
  if note `elem` (getKeys x) && octave == (getOctave x)
  then Just (snd x)
  else lookupDistance (note,octave) xs
  where getKeys x = fst $ fst x
        getOctave x = snd $ fst x

\end{code}

Now we can get the cost for a note given a key, the note we are searching for, and the octave.

The distance, or cost, between two notes is the distance in semitones between the two tones in the scale.

\begin{code}

getCost :: AbsNote -> AbsNote ->  Int 
getCost (n1,o1) (n2,o2) = abs $ (absPitch (n1,o1) - absPitch (n2,o2))

\end{code}

To calculate the cost of chords we apply the help of some helper functions, mainly to juggle our 
triads into different shapes and forms for conveniency.
 
\begin{code}
 
listOfTriplets :: Key -> Chord -> [(AbsNote, AbsNote, AbsNote)]
listOfTriplets key chord = concat $ makeAllPossibleChords key chord
 
triplets :: Key -> Chord -> [[AbsNote]]
triplets key chord = map toList (listOfTriplets key chord)

\end{code}

Here we create an associative table with triad chords as keys and their cost as values.

\begin{code}
 
costOfChords :: Key -> TriadChord -> Chord -> [(TriadChord, Cost)]
costOfChords key prevChord chord = zip (listOfTriplets key chord)
                                   $ map sum 
                                   $ map (\currentChord -> zipWith (getCost) (toList prevChord) currentChord)
                                   $ triplets key chord

\end{code}

The closest chord to another chord is done by searching for the least cost in the associative 
table created above.

\begin{code}
 
closestChord :: Ord a => [(t, a)] -> (t, a) -> t
closestChord [] (y,cost) = y
closestChord (x:xs) (y,cost) = if cost < (snd x)
                               then closestChord xs (y,cost)
                               else closestChord xs x

\end{code}

We summon the correct chord by calling upon the help of our minion utility functions mentioned above. 

\begin{code}
 
getChord :: Key -> TriadChord -> Chord -> TriadChord
getChord key prevChord chord = closestChord (listOfChords key prevChord chord) ((listOfChords key prevChord chord) !! 0)
  where listOfChords k pc c = (costOfChords k pc c)

\end{code}

Now that we are able to artistically select a beautiful chord given another, we can generate a list of 
chords for the whole song given a value that specifies whether we are at the beginning of the song, the 
song's key, and the chord progression of the song and a starting chord.

\begin{code}
 
mkVoices :: Bool -> Key -> ChordProgression -> TriadChord -> [TriadChord]
mkVoices _ _ [] _ = []
mkVoices isFirst key (x:xs) prevChord = case isFirst of
  True -> getChord key prevChord x : mkVoices False key xs (getChord key ((concat $ makeAllPossibleChords key x) !! 0) x)
  False -> getChord key prevChord x : mkVoices False key xs prevChord

\end{code}

And alas, we have reached the end of our last venture! Fully equipped, we are now able to generate 
philosophical music given a key and a chord progression. This is done by taking the chord progression, 
mapping it to lists, which maps a fold over the function that converts the absolute notes into playable 
music, to make the music play in parallell. Then we perform another fold that sequences the chords into 
the supreme accompaniment.

\begin{code}
 
autoVoice :: Key -> ChordProgression -> Music
autoVoice key chordProgression = foldr1 (:+:) $ map (\absNotes -> foldr1 (:=:) $ map noteToMusic absNotes) $ map toList $ chords
  where
    chords :: [TriadChord]
    chords = mkVoices True key chordProgression (getChord key (head $ concat $ makeAllPossibleChords key $ firstChord) $ firstChord)
    firstChord = head chordProgression
    noteToMusic :: (Note, AutoComp.Octave) -> Music
    noteToMusic (note, octave) = mapNote note (fromIntegral octave) hn volume

basic = Basic
calypso = Calypso
boogie = Boogie

\end{code}

We lied, here we actually tie up the last knot of our journey! autoComp will generate the bass line and 
accompaniment for the given bass style, key and chord progression.

\begin{code}
 
autoComp :: BassStyle -> Key -> [[Chord]] -> Music
autoComp style key chordProgression = autoBass style key chordProgression :=: (autoVoice key $ concat $ chordProgression)

\end{code}
 
Regards,
ada10jan && ada08ber

-- http://youtu.be/MfM9gQkfwyg?t=48s 
