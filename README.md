# music-tt

Music Theory Tool - music-tt

Run using:
```
stack run music-tt
```

Interactively:
```
stack ghci
ghci> import Music.Theory
ghci> pretty c'MajorKey
ghci> :q
```

# Idea

- Codify basic music theory concepts
- Codify readings from music theory books
- Implement a helper tool for playing instruments and composition (fretboard/piano visualisations, progressions generator, chords inversions and substitutions generation, exercises generation etc.)

# Motivation

It's an attempt to codify some basic music theory concepts.
I intentionally wasn't checking existing libraries (such as `haskore`) to be able to confront my ideas after implementation.
