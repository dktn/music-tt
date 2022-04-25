module Music.Theory
where

import           Music.Types

----- smart constructors

pc :: NoteName -> Accidentals -> PitchClass
pc noteName accidentals =
  PitchClass
  { noteName    = noteName
  , accidentals = accidentals
  }

note :: PitchClass -> Int -> Note
note pitchClass octaveNumber =
  Note
  { pitchClass = pitchClass
  , octave = Octave octaveNumber
  }

-- tools

reducePitchClass :: PitchClass -> PitchClass
reducePitchClass pitchClass = case accidentals pitchClass of
  NoAcc -> pitchClass
  Flat -> case noteName pitchClass of
    C -> pc B NoAcc
    F -> pc E NoAcc
    _ -> pitchClass
  Sharp -> case noteName pitchClass of
    B -> pc C NoAcc
    E -> pc F NoAcc
    _ -> pitchClass
  DoubleFlat -> case noteName pitchClass of
    C -> pc B Flat
    D -> pc C NoAcc
    E -> pc D NoAcc
    F -> pc E Flat
    G -> pc F NoAcc
    A -> pc G NoAcc
    B -> pc A NoAcc
  DoubleSharp -> case noteName pitchClass of
    C -> pc D NoAcc
    D -> pc E NoAcc
    E -> pc F Sharp
    F -> pc G NoAcc
    G -> pc A NoAcc
    A -> pc B NoAcc
    B -> pc C Sharp

shiftUpKey :: Key -> Key
shiftUpKey (Key d1 d2 d3 d4 d5 d6 d7) = Key d2 d3 d4 d5 d6 d7 d1

shiftDownKey :: Key -> Key
shiftDownKey (Key d1 d2 d3 d4 d5 d6 d7) = Key d7 d1 d2 d3 d4 d5 d6

majorToMinorKey :: Key -> Key
majorToMinorKey = shiftDownKey . shiftDownKey

minorToMajorKey :: Key -> Key
minorToMajorKey = shiftUpKey . shiftUpKey

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f = foldr (.) id (replicate n f)

startFromKey :: Int -> Key -> Key
startFromKey num
  | num > 0 = applyNTimes (( num - 1) `mod` 7) shiftUpKey
  | num < 0 = applyNTimes ((-num - 1) `mod` 7) shiftDownKey
  | otherwise = id

-- constants

c, d, e, f, g, a, b :: PitchClass
c = pc C NoAcc
d = pc D NoAcc
e = pc E NoAcc
f = pc F NoAcc
g = pc G NoAcc
a = pc A NoAcc
b = pc B NoAcc

c', d', e', f', g', a', b' :: PitchClass
c' = pc C Sharp
d' = pc D Sharp
e' = pc E Sharp
f' = pc F Sharp
g' = pc G Sharp
a' = pc A Sharp
b' = pc B Sharp

c_, d_, e_, f_, g_, a_, b_ :: PitchClass
c_ = pc C Flat
d_ = pc D Flat
e_ = pc E Flat
f_ = pc F Flat
g_ = pc G Flat
a_ = pc A Flat
b_ = pc B Flat

cMajorKey :: Key
cMajorKey  = Key c  d  e  f  g  a  b

gMajorKey, dMajorKey, aMajorKey, eMajorKey, bMajorKey, f'MajorKey, c'MajorKey :: Key
gMajorKey  = startFromKey 5 $ Key c  d  e  f' g  a  b
dMajorKey  = startFromKey 2 $ Key c' d  e  f' g  a  b
aMajorKey  = startFromKey 6 $ Key c' d  e  f' g' a  b
eMajorKey  = startFromKey 3 $ Key c' d' e  f' g' a  b
bMajorKey  = startFromKey 7 $ Key c' d' e  f' g' a' b
f'MajorKey = startFromKey 4 $ Key c' d' e' f' g' a' b
c'MajorKey = startFromKey 8 $ Key c' d' e' f' g' a' b'

fMajorKey, b_MajorKey, e_MajorKey, a_MajorKey, d_MajorKey, g_MajorKey, c_MajorKey :: Key
fMajorKey  = startFromKey 4 $ Key c  d  e  f  g  a  b_
b_MajorKey = startFromKey 7 $ Key c  d  e_ f  g  a  b_
e_MajorKey = startFromKey 3 $ Key c  d  e_ f  g  a_ b_
a_MajorKey = startFromKey 6 $ Key c  d_ e_ f  g  a_ b_
d_MajorKey = startFromKey 2 $ Key c  d_ e_ f  g_ a_ b_
g_MajorKey = startFromKey 5 $ Key c_ d_ e_ f  g_ a_ b_
c_MajorKey = startFromKey 1 $ Key c_ d_ e_ f_ g_ a_ b_

aMinorKey, eMinorKey :: Key
aMinorKey = majorToMinorKey cMajorKey
eMinorKey = majorToMinorKey gMajorKey
-- TODO: rest

middleC :: Note
middleC = note c 4

-- tests

test :: IO ()
test = do
  putStrLn $ "middle C: " <> pretty middleC
  putStrLn $ "C# chord notes: " <> pretty c' <> " " <> pretty e' <> " " <> pretty g'
  putStrLn $ "Bb chord notes: " <> pretty b_ <> " " <> pretty d_ <> " " <> pretty f_
  putStrLn $ "B  Major Key: " <> pretty bMajorKey
  putStrLn $ "Gb Major Key: " <> pretty g_MajorKey
  putStrLn $ "A  Minor Key: " <> pretty aMinorKey
