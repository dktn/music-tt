{-# LANGUAGE FlexibleInstances #-}
module Music.Types
where

data NoteName =
    A
  | B
  | C
  | D
  | E
  | F
  | G
  deriving (Show, Eq, Ord)

newtype Octave = Octave { octaveNumber :: Int }
  deriving (Show, Eq, Ord)

data Accidentals =
    NoAcc
  | Flat
  | DoubleFlat
  | Sharp
  | DoubleSharp
  deriving (Show, Eq, Ord)

data PitchClass =
  PitchClass
  { noteName    :: NoteName
  , accidentals :: Accidentals
  }
  deriving (Show, Eq, Ord)

data Note =
  Note
  { pitchClass :: PitchClass
  , octave     :: Octave
  }
  deriving (Show, Eq, Ord)

data Key =
  Key
  { degree1 :: PitchClass
  , degree2 :: PitchClass
  , degree3 :: PitchClass
  , degree4 :: PitchClass
  , degree5 :: PitchClass
  , degree6 :: PitchClass
  , degree7 :: PitchClass
  }
  deriving (Show, Eq, Ord)



-- varia

class Pretty a where
  pretty :: a -> String

instance Pretty Accidentals where
  pretty NoAcc       = ""
  pretty Flat        = "b"
  pretty DoubleFlat  = "bb"
  pretty Sharp       = "#"
  pretty DoubleSharp = "##"

instance Pretty NoteName where
  pretty = show

instance Pretty PitchClass where
  pretty (PitchClass n a)= pretty n <> pretty a

instance Pretty Octave where
  pretty (Octave o) = show o

instance Pretty Note where
  pretty (Note pc o) = pretty pc <> pretty o

instance Pretty Key where
  pretty (Key d1 d2 d3 d4 d5 d6 d7) =
    "["  <> pretty d1 <>
    ", " <> pretty d2 <>
    ", " <> pretty d3 <>
    ", " <> pretty d4 <>
    ", " <> pretty d5 <>
    ", " <> pretty d6 <>
    ", " <> pretty d7 <>
    "]"
